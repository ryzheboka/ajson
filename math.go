package ajson

import (
	"encoding/binary"
	"errors"
	"math"
	"math/rand"
	"regexp"
	"sort"
	"strconv"
	"strings"
)

// Function - internal left function of JSONPath
type Function func(node *Node) (result *Node, err error)

// Operation - internal script operation of JSONPath
type Operation func(left *Node, right *Node) (result *Node, err error)

var (
	// Operator precedence
	// From https://golang.org/ref/spec#Operator_precedence
	//
	//	Precedence    Operator
	//	    5             *  /  %  <<  >>  &  &^
	//	    4             +  -  |  ^
	//	    3             ==  !=  <  <=  >  >= =~
	//	    2             &&
	//	    1             ||
	//
	// Arithmetic operators
	// From https://golang.org/ref/spec#Arithmetic_operators
	//
	//	+    sum                    integers, floats, complex values, strings
	//	-    difference             integers, floats, complex values
	//	*    product                integers, floats, complex values
	//	/    quotient               integers, floats, complex values
	//	%    remainder              integers
	//
	//	&    bitwise AND            integers
	//	|    bitwise OR             integers
	//	^    bitwise XOR            integers
	//	&^   bit clear (AND NOT)    integers
	//
	//	<<   left shift             integer << unsigned integer
	//	>>   right shift            integer >> unsigned integer
	//
	//	==  equals                  any
	//	!=  not equals              any
	//	<   less                    any
	//	<=  less or equals          any
	//	>   larger                  any
	//	>=  larger or equals        any
	//	=~  equals regex string     strings
	//
	priority = map[string]uint8{
		"**": 6, // additional: power
		"*":  5,
		"/":  5,
		"%":  5,
		"<<": 5,
		">>": 5,
		"&":  5,
		"&^": 5,
		"+":  4,
		"-":  4,
		"|":  4,
		"^":  4,
		"==": 3,
		"!=": 3,
		"<":  3,
		"<=": 3,
		">":  3,
		">=": 3,
		"=~": 3,
		"&&": 2,
		"||": 1,
	}
	priorityChar = map[byte]bool{
		'*': true,
		'/': true,
		'%': true,
		'<': true,
		'>': true,
		'&': true,
		'|': true,
		'^': true,
		'+': true,
		'-': true,
		'=': true,
		'!': true,
	}

	rightOp = map[string]bool{
		"**": true,
	}

	operations = map[string]Operation{
		"**": func(left *Node, right *Node) (result *Node, err error) {
			lnum, rnum, err := _floats(left, right)
			if err != nil {
				return
			}
			return valueNode(nil, "power", Numeric, math.Pow(lnum, rnum)), nil
		},
		"*": func(left *Node, right *Node) (result *Node, err error) {
			lnum, rnum, err := _floats(left, right)
			if err != nil {
				return
			}
			return valueNode(nil, "multiply", Numeric, float64(lnum*rnum)), nil
		},
		"/": func(left *Node, right *Node) (result *Node, err error) {
			lnum, rnum, err := _floats(left, right)
			if err != nil {
				return
			}
			if rnum == 0 {
				return nil, errorRequest("division by zero")
			}
			return valueNode(nil, "division", Numeric, float64(lnum/rnum)), nil
		},
		"%": func(left *Node, right *Node) (result *Node, err error) {
			lnum, err := left.getInteger()
			if err != nil {
				return
			}
			rnum, err := right.getInteger()
			if err != nil {
				return
			}
			return valueNode(nil, "remainder", Numeric, float64(lnum%rnum)), nil
		},
		"<<": func(left *Node, right *Node) (result *Node, err error) {
			lnum, err := left.getInteger()
			if err != nil {
				return
			}
			rnum, err := right.getUInteger()
			if err != nil {
				return
			}
			return valueNode(nil, "left shift", Numeric, float64(lnum<<rnum)), nil
		},
		">>": func(left *Node, right *Node) (result *Node, err error) {
			lnum, err := left.getInteger()
			if err != nil {
				return
			}
			rnum, err := right.getUInteger()
			if err != nil {
				return
			}
			return valueNode(nil, "right shift", Numeric, float64(lnum>>rnum)), nil
		},
		"&": func(left *Node, right *Node) (result *Node, err error) {
			lnum, rnum, err := _ints(left, right)
			if err != nil {
				return
			}
			return valueNode(nil, "bitwise AND", Numeric, float64(lnum&rnum)), nil
		},
		"&^": func(left *Node, right *Node) (result *Node, err error) {
			lnum, rnum, err := _ints(left, right)
			if err != nil {
				return
			}
			return valueNode(nil, "bit clear (AND NOT)", Numeric, float64(lnum&^rnum)), nil
		},
		"+": func(left *Node, right *Node) (result *Node, err error) {
			if left.IsString() {
				lnum, rnum, err := _strings(left, right)
				if err != nil {
					return nil, err
				}
				return valueNode(nil, "sum", String, lnum+rnum), nil
			}
			lnum, rnum, err := _floats(left, right)
			if err != nil {
				return nil, err
			}
			return valueNode(nil, "sum", Numeric, float64(lnum+rnum)), nil
		},
		"-": func(left *Node, right *Node) (result *Node, err error) {
			lnum, rnum, err := _floats(left, right)
			if err != nil {
				return
			}
			return valueNode(nil, "sub", Numeric, float64(lnum-rnum)), nil
		},
		"|": func(left *Node, right *Node) (result *Node, err error) {
			lnum, rnum, err := _ints(left, right)
			if err != nil {
				return
			}
			return valueNode(nil, "bitwise OR", Numeric, float64(lnum|rnum)), nil
		},
		"^": func(left *Node, right *Node) (result *Node, err error) {
			lnum, rnum, err := _ints(left, right)
			if err != nil {
				return nil, err
			}
			return valueNode(nil, "bitwise XOR", Numeric, float64(lnum^rnum)), nil
		},
		"==": func(left *Node, right *Node) (result *Node, err error) {
			res, err := left.Eq(right)
			if err != nil {
				return nil, err
			}
			return valueNode(nil, "eq", Bool, res), nil
		},
		"!=": func(left *Node, right *Node) (result *Node, err error) {
			res, err := left.Eq(right)
			if err != nil {
				return nil, err
			}
			return valueNode(nil, "neq", Bool, !res), nil
		},
		"=~": func(left *Node, right *Node) (node *Node, err error) {
			pattern, err := right.GetString()
			if err != nil {
				return nil, err
			}
			val, err := left.GetString()
			if err != nil {
				return nil, err
			}
			res, err := regexp.MatchString(pattern, val)
			if err != nil {
				return nil, err
			}
			return valueNode(nil, "eq", Bool, res), nil
		},
		"<": func(left *Node, right *Node) (result *Node, err error) {
			res, err := left.Le(right)
			if err != nil {
				return nil, err
			}
			return valueNode(nil, "le", Bool, bool(res)), nil
		},
		"<=": func(left *Node, right *Node) (result *Node, err error) {
			res, err := left.Leq(right)
			if err != nil {
				return nil, err
			}
			return valueNode(nil, "leq", Bool, bool(res)), nil
		},
		">": func(left *Node, right *Node) (result *Node, err error) {
			res, err := left.Ge(right)
			if err != nil {
				return nil, err
			}
			return valueNode(nil, "ge", Bool, bool(res)), nil
		},
		">=": func(left *Node, right *Node) (result *Node, err error) {
			res, err := left.Geq(right)
			if err != nil {
				return nil, err
			}
			return valueNode(nil, "geq", Bool, bool(res)), nil
		},
		"&&": func(left *Node, right *Node) (result *Node, err error) {
			res := false
			lval, err := boolean(left)
			if err != nil {
				return nil, err
			}
			if lval {
				rval, err := boolean(right)
				if err != nil {
					return nil, err
				}
				res = rval
			}
			return valueNode(nil, "AND", Bool, bool(res)), nil
		},
		"||": func(left *Node, right *Node) (result *Node, err error) {
			res := true
			lval, err := boolean(left)
			if err != nil {
				return nil, err
			}
			if !lval {
				rval, err := boolean(right)
				if err != nil {
					return nil, err
				}
				res = rval
			}
			return valueNode(nil, "OR", Bool, bool(res)), nil
		},
	}

	randFunc    = rand.Float64
	randIntFunc = rand.Intn

	functions = map[string]Function{
		"abs":         numericFunction("Abs", math.Abs),
		"acos":        numericFunction("Acos", math.Acos),
		"acosh":       numericFunction("Acosh", math.Acosh),
		"asin":        numericFunction("Asin", math.Asin),
		"asinh":       numericFunction("Asinh", math.Asinh),
		"atan":        numericFunction("Atan", math.Atan),
		"atanh":       numericFunction("Atanh", math.Atanh),
		"cbrt":        numericFunction("Cbrt", math.Cbrt),
		"ceil":        numericFunction("Ceil", math.Ceil),
		"cos":         numericFunction("Cos", math.Cos),
		"cosh":        numericFunction("Cosh", math.Cosh),
		"erf":         numericFunction("Erf", math.Erf),
		"erfc":        numericFunction("Erfc", math.Erfc),
		"erfcinv":     numericFunction("Erfcinv", math.Erfcinv),
		"erfinv":      numericFunction("Erfinv", math.Erfinv),
		"exp":         numericFunction("Exp", math.Exp),
		"exp2":        numericFunction("Exp2", math.Exp2),
		"expm1":       numericFunction("Expm1", math.Expm1),
		"floor":       numericFunction("Floor", math.Floor),
		"gamma":       numericFunction("Gamma", math.Gamma),
		"j0":          numericFunction("J0", math.J0),
		"j1":          numericFunction("J1", math.J1),
		"log":         numericFunction("Log", math.Log),
		"log10":       numericFunction("Log10", math.Log10),
		"log1p":       numericFunction("Log1p", math.Log1p),
		"log2":        numericFunction("Log2", math.Log2),
		"logb":        numericFunction("Logb", math.Logb),
		"round":       numericFunction("Round", math.Round),
		"roundtoeven": numericFunction("RoundToEven", math.RoundToEven),
		"sin":         numericFunction("Sin", math.Sin),
		"sinh":        numericFunction("Sinh", math.Sinh),
		"sqrt":        numericFunction("Sqrt", math.Sqrt),
		"tan":         numericFunction("Tan", math.Tan),
		"tanh":        numericFunction("Tanh", math.Tanh),
		"trunc":       numericFunction("Trunc", math.Trunc),
		"y0":          numericFunction("Y0", math.Y0),
		"y1":          numericFunction("Y1", math.Y1),

		"pow10": func(node *Node) (result *Node, err error) {
			num, err := node.getInteger()
			if err != nil {
				return
			}
			return valueNode(nil, "Pow10", Numeric, float64(math.Pow10(num))), nil
		},
		"length": func(node *Node) (result *Node, err error) {
			if node.IsArray() {
				return valueNode(nil, "length", Numeric, float64(node.Size())), nil
			}
			if node.IsObject() {
				return valueNode(nil, "length", Numeric, float64(node.Size())), nil
			}
			if node.IsString() {
				if res, err := node.GetString(); err != nil {
					return nil, err
				} else {
					return valueNode(nil, "length", Numeric, float64(len(res))), nil
				}
			}
			return valueNode(nil, "length", Numeric, float64(1)), nil
		},
		"factorial": func(node *Node) (result *Node, err error) {
			num, err := node.getUInteger()
			if err != nil {
				return
			}
			return valueNode(nil, "factorial", Numeric, float64(mathFactorial(num))), nil
		},
		"avg": func(node *Node) (result *Node, err error) {
			if node.isContainer() {
				sum := float64(0)
				if node.Size() == 0 {
					return valueNode(nil, "avg", Numeric, sum), nil
				}
				var value float64
				for _, temp := range node.Inheritors() {
					value, err = temp.GetNumeric()
					if err != nil {
						return nil, err
					}
					sum += value
				}
				return valueNode(nil, "avg", Numeric, sum/float64(node.Size())), nil
			}
			return valueNode(nil, "avg", Null, nil), nil
		},
		/*
		 b64encode implementation is derived from the encoding/base64 Encode method.
		 The implementation can be found here:
		 https://cs.opensource.google/go/go/+/refs/tags/go1.19.2:src/encoding/base64/base64.go;l=140;drc=49abdbccde5de042997d6aabe7819212b88f2ef5
		*/
		"b64encode": func(node *Node) (result *Node, err error) {
			if node.IsString() {
				if _, err := node.GetString(); err != nil {
					return nil, err
				} else {
					encode := []byte("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
					sourceString, _ := node.GetString()
					sourceBytes := []byte(sourceString)
					if len(sourceBytes) == 0 {
						return nil, errors.New("String is empty")
					}

					di, si := 0, 0
					n := (len(sourceBytes) / 3) * 3
					remain := len(sourceBytes) % 3
					padding := 0
					if remain != 0 {
						padding = 4
					}
					result := make([]byte, n/3*4+padding)
					for si < n {
						// Convert 3x 8bit source bytes into 4 bytes
						val := uint(sourceBytes[si+0])<<16 | uint(sourceBytes[si+1])<<8 | uint(sourceBytes[si+2])

						result[di+0] = encode[val>>18&0x3F]
						result[di+1] = encode[val>>12&0x3F]
						result[di+2] = encode[val>>6&0x3F]
						result[di+3] = encode[val&0x3F]

						si += 3
						di += 4
					}
					if remain == 0 {
						return valueNode(nil, "base64_encode", String, string(result)), nil
					}
					// Add the remaining small block
					val := uint(sourceBytes[si+0]) << 16
					if remain == 2 {
						val |= uint(sourceBytes[si+1]) << 8
					}

					result[di+0] = encode[val>>18&0x3F]
					result[di+1] = encode[val>>12&0x3F]

					switch remain {
					case 2:
						result[di+2] = encode[val>>6&0x3F]
						result[di+3] = '='

					case 1:
						result[di+2] = byte('=')
						result[di+3] = byte('=')

					}
					return valueNode(nil, "base64_encode", String, string(result)), nil
				}
			}
			return valueNode(nil, "base64_encode", Null, nil), nil
		},
		"b64decode": func(node *Node) (result *Node, err error) {
			if node.IsString() {
				if _, err := node.GetString(); err != nil {
					return nil, err
				} else {
					n := 0
					sourceString, _ := node.GetString()
					sourceBytes := []byte(sourceString)
					result := make([]byte, len(sourceBytes)/4*3)
					decodeMap := getDecodeMap()

					si := 0
					for strconv.IntSize >= 64 && len(sourceBytes)-si >= 8 && len(result)-n >= 8 {
						src2 := sourceBytes[si : si+8]
						if dn, ok := assemble64(
							decodeMap[src2[0]],
							decodeMap[src2[1]],
							decodeMap[src2[2]],
							decodeMap[src2[3]],
							decodeMap[src2[4]],
							decodeMap[src2[5]],
							decodeMap[src2[6]],
							decodeMap[src2[7]],
						); ok {
							binary.BigEndian.PutUint64(result[n:], dn)
							n += 6
							si += 8
						} else {
							var ninc int
							si, ninc, err = decodeQuantum(result[n:], sourceBytes, si)
							n += ninc
							if err != nil {
								return nil, err
							}
						}
					}

					for len(sourceBytes)-si >= 4 && len(result)-n >= 4 {
						src2 := sourceBytes[si : si+4]
						if dn, ok := assemble32(
							decodeMap[src2[0]],
							decodeMap[src2[1]],
							decodeMap[src2[2]],
							decodeMap[src2[3]],
						); ok {
							binary.BigEndian.PutUint32(result[n:], dn)
							n += 3
							si += 4
						} else {
							var ninc int
							si, ninc, err = decodeQuantum(result[n:], sourceBytes, si)
							n += ninc
							if err != nil {
								return nil, err
							}
						}
					}

					for si < len(sourceBytes) {
						var ninc int
						si, ninc, err = decodeQuantum(result[n:], sourceBytes, si)
						n += ninc
						if err != nil {
							return nil, err
						}
					}
					return valueNode(nil, "base64_decode", String, string(result[:n])), nil
				}
				return valueNode(nil, "base64_encode", String, result), nil

			}
			return valueNode(nil, "base64_encode", Null, nil), nil
		},
		"sum": func(node *Node) (result *Node, err error) {
			if node.isContainer() {
				sum := float64(0)
				if node.Size() == 0 {
					return valueNode(nil, "sum", Numeric, sum), nil
				}
				var value float64
				for _, temp := range node.Inheritors() {
					value, err = temp.GetNumeric()
					if err != nil {
						return nil, err
					}
					sum += value
				}
				return valueNode(nil, "sum", Numeric, sum), nil
			}
			return valueNode(nil, "sum", Null, nil), nil
		},
		"not": func(node *Node) (result *Node, err error) {
			if value, err := boolean(node); err != nil {
				return nil, err
			} else {
				return valueNode(nil, "not", Bool, !value), nil
			}
		},
		"rand": func(node *Node) (result *Node, err error) {
			num, err := node.GetNumeric()
			if err != nil {
				return
			}
			return valueNode(nil, "Rand", Numeric, randFunc()*num), nil
		},
		"randint": func(node *Node) (result *Node, err error) {
			num, err := node.getInteger()
			if err != nil {
				return
			}
			return valueNode(nil, "RandInt", Numeric, float64(randIntFunc(num))), nil
		},
	}

	constants = map[string]*Node{
		"e":   valueNode(nil, "e", Numeric, float64(math.E)),
		"pi":  valueNode(nil, "pi", Numeric, float64(math.Pi)),
		"phi": valueNode(nil, "phi", Numeric, float64(math.Phi)),

		"sqrt2":   valueNode(nil, "sqrt2", Numeric, float64(math.Sqrt2)),
		"sqrte":   valueNode(nil, "sqrte", Numeric, float64(math.SqrtE)),
		"sqrtpi":  valueNode(nil, "sqrtpi", Numeric, float64(math.SqrtPi)),
		"sqrtphi": valueNode(nil, "sqrtphi", Numeric, float64(math.SqrtPhi)),

		"ln2":    valueNode(nil, "ln2", Numeric, float64(math.Ln2)),
		"log2e":  valueNode(nil, "log2e", Numeric, float64(math.Log2E)),
		"ln10":   valueNode(nil, "ln10", Numeric, float64(math.Ln10)),
		"log10e": valueNode(nil, "log10e", Numeric, float64(math.Log10E)),

		"true":  valueNode(nil, "true", Bool, true),
		"false": valueNode(nil, "false", Bool, false),
		"null":  valueNode(nil, "null", Null, nil),
	}
)

// AddFunction add a function for internal JSONPath script
func AddFunction(alias string, function Function) {
	functions[strings.ToLower(alias)] = function
}

// AddOperation add an operation for internal JSONPath script
func AddOperation(alias string, prior uint8, right bool, operation Operation) {
	alias = strings.ToLower(alias)
	operations[alias] = operation
	priority[alias] = prior
	priorityChar[alias[0]] = true
	if right {
		rightOp[alias] = true
	}
}

// AddConstant add a constant for internal JSONPath script
func AddConstant(alias string, value *Node) {
	constants[strings.ToLower(alias)] = value
}

func numericFunction(name string, fn func(float float64) float64) Function {
	return func(node *Node) (result *Node, err error) {
		if node.IsNumeric() {
			num, err := node.GetNumeric()
			if err != nil {
				return nil, err
			}
			return valueNode(nil, name, Numeric, fn(num)), nil
		}
		return nil, errorRequest("function '%s' was called from non numeric node", name)
	}
}

func mathFactorial(x uint) uint {
	if x == 0 {
		return 1
	}
	return x * mathFactorial(x-1)
}

func comparisonOperationsOrder() []string {
	result := make([]string, 0, len(operations))
	for operation := range operations {
		result = append(result, operation)
	}

	sort.Slice(result, func(i, j int) bool {
		return len(result[i]) > len(result[j])
	})
	return result
}

func assemble32(n1, n2, n3, n4 byte) (dn uint32, ok bool) {
	// Check that all the digits are valid. If any of them was 0xff, their
	// bitwise OR will be 0xff.
	if n1|n2|n3|n4 == 0xff {
		return 0, false
	}
	return uint32(n1)<<26 |
			uint32(n2)<<20 |
			uint32(n3)<<14 |
			uint32(n4)<<8,
		true
}
func getDecodeMap() []byte {
	encoder := []byte("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

	decodeMap := []byte(
		"\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff" +
			"\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff" +
			"\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff" +
			"\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff" +
			"\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff" +
			"\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff" +
			"\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff" +
			"\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff" +
			"\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff" +
			"\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff" +
			"\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff" +
			"\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff" +
			"\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff" +
			"\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff" +
			"\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff" +
			"\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff")

	// ... TODO: implementation
	for i := 0; i < 64; i++ {
		decodeMap[encoder[i]] = byte(i)
	}
	return decodeMap
}

func decodeQuantum(dst, src []byte, si int) (nsi, n int, err error) {
	// Decode quantum using the base64 alphabet
	var dbuf [4]byte
	dlen := 4
	decodeMap := getDecodeMap()
	// Lift the nil check outside of the loop.

	for j := 0; j < len(dbuf); j++ {
		if len(src) == si {
			switch {
			case j == 0:
				return si, 0, nil
			case j == 1:
				return si, 0, errors.New("wrong")
			}
			dlen = j
			break
		}
		in := src[si]
		si++

		out := decodeMap[in]
		if out != 0xff {
			dbuf[j] = out
			continue
		}

		if in == '\n' || in == '\r' {
			j--
			continue
		}

		if rune(in) != '=' {
			return si, 0, errors.New("Wrong")
		}

		// We've reached the end and there's padding
		switch j {
		case 0, 1:
			// incorrect padding
			return si, 0, errors.New("Wrong")
		case 2:
			// "==" is expected, the first "=" is already consumed.
			// skip over newlines
			for si < len(src) && (src[si] == '\n' || src[si] == '\r') {
				si++
			}
			if si == len(src) {
				// not enough padding
				return si, 0, errors.New("Wrong")
			}
			if rune(src[si]) != '=' {
				// incorrect padding
				return si, 0, errors.New("Wrong")
			}

			si++
		}

		// skip over newlines
		for si < len(src) && (src[si] == '\n' || src[si] == '\r') {
			si++
		}
		if si < len(src) {
			// trailing garbage
			err = errors.New("Wrong")
		}
		dlen = j
		break
	}

	// Convert 4x 6bit source bytes into 3 bytes
	val := uint(dbuf[0])<<18 | uint(dbuf[1])<<12 | uint(dbuf[2])<<6 | uint(dbuf[3])
	dbuf[2], dbuf[1], dbuf[0] = byte(val>>0), byte(val>>8), byte(val>>16)
	switch dlen {
	case 4:
		dst[2] = dbuf[2]
		dbuf[2] = 0
		fallthrough
	case 3:
		dst[1] = dbuf[1]
		dbuf[1] = 0
		fallthrough
	case 2:
		dst[0] = dbuf[0]
	}

	return si, dlen - 1, err
}

func assemble64(n1, n2, n3, n4, n5, n6, n7, n8 byte) (dn uint64, ok bool) {
	// Check that all the digits are valid. If any of them was 0xff, their
	// bitwise OR will be 0xff.
	if n1|n2|n3|n4|n5|n6|n7|n8 == 0xff {
		return 0, false
	}
	return uint64(n1)<<58 |
			uint64(n2)<<52 |
			uint64(n3)<<46 |
			uint64(n4)<<40 |
			uint64(n5)<<34 |
			uint64(n6)<<28 |
			uint64(n7)<<22 |
			uint64(n8)<<16,
		true
}
