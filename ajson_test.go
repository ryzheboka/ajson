package ajson

import (
	"bytes"
	"testing"
)

type testCase struct {
	name  string
	input []byte
	_type NodeType
	value []byte
}

func simpleValid(test *testCase, t *testing.T) {
	root, err := Unmarshal(test.input, false)
	if err != nil {
		t.Errorf("Error on Unmarshal(%s): %s", test.name, err.Error())
	} else if root == nil {
		t.Errorf("Error on Unmarshal(%s): root is nil", test.name)
	} else if root.Type() != test._type {
		t.Errorf("Error on Unmarshal(%s): wrong type", test.name)
	} else if !bytes.Equal(root.Source(), test.value) {
		t.Errorf("Error on Unmarshal(%s): %s != %s", test.name, root.Source(), test.value)
	}
}

func simpleInvalid(test *testCase, t *testing.T) {
	root, err := Unmarshal(test.input, false)
	if err == nil {
		t.Errorf("Error on Unmarshal(%s): error expected", test.name)
	} else if root != nil {
		t.Errorf("Error on Unmarshal(%s): root is not nil", test.name)
	}
}

func TestUnmarshal_NumericSimpleSuccess(t *testing.T) {
	tests := []testCase{
		{name: "1", input: []byte("1"), _type: Numeric, value: []byte("1")},
		{name: "+1", input: []byte("+1"), _type: Numeric, value: []byte("+1")},
		{name: "-1", input: []byte("-1"), _type: Numeric, value: []byte("-1")},

		{name: "1234567890", input: []byte("1234567890"), _type: Numeric, value: []byte("1234567890")},
		{name: "+123", input: []byte("+123"), _type: Numeric, value: []byte("+123")},
		{name: "-123", input: []byte("-123"), _type: Numeric, value: []byte("-123")},

		{name: "123.456", input: []byte("123.456"), _type: Numeric, value: []byte("123.456")},
		{name: "+123.456", input: []byte("+123.456"), _type: Numeric, value: []byte("+123.456")},
		{name: "-123.456", input: []byte("-123.456"), _type: Numeric, value: []byte("-123.456")},

		{name: ".456", input: []byte(".456"), _type: Numeric, value: []byte(".456")},
		{name: "+.456", input: []byte("+.456"), _type: Numeric, value: []byte("+.456")},
		{name: "-.456", input: []byte("-.456"), _type: Numeric, value: []byte("-.456")},

		{name: "1e3", input: []byte("1e3"), _type: Numeric, value: []byte("1e3")},
		{name: "1e+3", input: []byte("1e+3"), _type: Numeric, value: []byte("1e+3")},
		{name: "1e-3", input: []byte("1e-3"), _type: Numeric, value: []byte("1e-3")},
		{name: "+1e3", input: []byte("+1e3"), _type: Numeric, value: []byte("+1e3")},
		{name: "+1e+3", input: []byte("+1e+3"), _type: Numeric, value: []byte("+1e+3")},
		{name: "+1e-3", input: []byte("+1e-3"), _type: Numeric, value: []byte("+1e-3")},
		{name: "-1e3", input: []byte("-1e3"), _type: Numeric, value: []byte("-1e3")},
		{name: "-1e+3", input: []byte("-1e+3"), _type: Numeric, value: []byte("-1e+3")},
		{name: "-1e-3", input: []byte("-1e-3"), _type: Numeric, value: []byte("-1e-3")},

		{name: "1.123e3.456", input: []byte("1.123e3.456"), _type: Numeric, value: []byte("1.123e3.456")},
		{name: "1.123e+3.456", input: []byte("1.123e+3.456"), _type: Numeric, value: []byte("1.123e+3.456")},
		{name: "1.123e-3.456", input: []byte("1.123e-3.456"), _type: Numeric, value: []byte("1.123e-3.456")},
		{name: "+1.123e3.456", input: []byte("+1.123e3.456"), _type: Numeric, value: []byte("+1.123e3.456")},
		{name: "+1.123e+3.456", input: []byte("+1.123e+3.456"), _type: Numeric, value: []byte("+1.123e+3.456")},
		{name: "+1.123e-3.456", input: []byte("+1.123e-3.456"), _type: Numeric, value: []byte("+1.123e-3.456")},
		{name: "-1.123e3.456", input: []byte("-1.123e3.456"), _type: Numeric, value: []byte("-1.123e3.456")},
		{name: "-1.123e+3.456", input: []byte("-1.123e+3.456"), _type: Numeric, value: []byte("-1.123e+3.456")},
		{name: "-1.123e-3.456", input: []byte("-1.123e-3.456"), _type: Numeric, value: []byte("-1.123e-3.456")},

		{name: "1E3", input: []byte("1E3"), _type: Numeric, value: []byte("1E3")},
		{name: "1E+3", input: []byte("1E+3"), _type: Numeric, value: []byte("1E+3")},
		{name: "1E-3", input: []byte("1E-3"), _type: Numeric, value: []byte("1E-3")},
		{name: "+1E3", input: []byte("+1E3"), _type: Numeric, value: []byte("+1E3")},
		{name: "+1E+3", input: []byte("+1E+3"), _type: Numeric, value: []byte("+1E+3")},
		{name: "+1E-3", input: []byte("+1E-3"), _type: Numeric, value: []byte("+1E-3")},
		{name: "-1E3", input: []byte("-1E3"), _type: Numeric, value: []byte("-1E3")},
		{name: "-1E+3", input: []byte("-1E+3"), _type: Numeric, value: []byte("-1E+3")},
		{name: "-1E-3", input: []byte("-1E-3"), _type: Numeric, value: []byte("-1E-3")},

		{name: "1.123E3.456", input: []byte("1.123E3.456"), _type: Numeric, value: []byte("1.123E3.456")},
		{name: "1.123E+3.456", input: []byte("1.123E+3.456"), _type: Numeric, value: []byte("1.123E+3.456")},
		{name: "1.123E-3.456", input: []byte("1.123E-3.456"), _type: Numeric, value: []byte("1.123E-3.456")},
		{name: "+1.123E3.456", input: []byte("+1.123E3.456"), _type: Numeric, value: []byte("+1.123E3.456")},
		{name: "+1.123E+3.456", input: []byte("+1.123E+3.456"), _type: Numeric, value: []byte("+1.123E+3.456")},
		{name: "+1.123E-3.456", input: []byte("+1.123E-3.456"), _type: Numeric, value: []byte("+1.123E-3.456")},
		{name: "-1.123E3.456", input: []byte("-1.123E3.456"), _type: Numeric, value: []byte("-1.123E3.456")},
		{name: "-1.123E+3.456", input: []byte("-1.123E+3.456"), _type: Numeric, value: []byte("-1.123E+3.456")},
		{name: "-1.123E-3.456", input: []byte("-1.123E-3.456"), _type: Numeric, value: []byte("-1.123E-3.456")},

		{name: "-1.123E-3.456 with spaces", input: []byte(" \r -1.123E-3.456 \t\n"), _type: Numeric, value: []byte("-1.123E-3.456")},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			simpleValid(&test, t)
		})
	}
}

func TestUnmarshal_NumericSimpleCorrupted(t *testing.T) {
	tests := []testCase{
		{name: "x1", input: []byte("x1")},
		{name: "1+1", input: []byte("1+1")},
		{name: "-1+", input: []byte("-1+")},
		{name: ".", input: []byte(".")},
		{name: "-", input: []byte("-")},
		{name: "+", input: []byte("+")},
		{name: "-.", input: []byte("-")},
		{name: "+.", input: []byte("+")},
		{name: "e", input: []byte("e")},
		{name: "e+", input: []byte("e+")},
		{name: "e+1-", input: []byte("e+1-")},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			simpleInvalid(&test, t)
		})
	}
}

func TestUnmarshal_StringSimpleSuccess(t *testing.T) {
	tests := []testCase{
		{name: "blank", input: []byte("\"\""), _type: String, value: []byte("\"\"")},
		{name: "char", input: []byte("\"c\""), _type: String, value: []byte("\"c\"")},
		{name: "word", input: []byte("\"cat\""), _type: String, value: []byte("\"cat\"")},
		{name: "spaces", input: []byte("  \"good cat\n\tor dog\"\r\n "), _type: String, value: []byte("\"good cat\n\tor dog\"")},
		{name: "backslash", input: []byte("\"good \\\"cat\\\"\""), _type: String, value: []byte("\"good \\\"cat\\\"\"")},
		{name: "backslash 2", input: []byte("\"good \\\\\\\"cat\\\"\""), _type: String, value: []byte("\"good \\\\\\\"cat\\\"\"")},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			simpleValid(&test, t)
		})
	}
}

func TestUnmarshal_StringSimpleCorrupted(t *testing.T) {
	tests := []testCase{
		{name: "one quote", input: []byte("\"")},
		{name: "one quote char", input: []byte("\"c")},
		{name: "wrong quotes", input: []byte("'cat'")},
		{name: "quotes in quotes", input: []byte("\"good \"cat\"\"")},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			simpleInvalid(&test, t)
		})
	}
}

func TestUnmarshal_NullSimpleSuccess(t *testing.T) {
	tests := []testCase{
		{name: "lower", input: []byte("null"), _type: Null, value: []byte("null")},
		{name: "upper", input: []byte("NULL"), _type: Null, value: []byte("NULL")},
		{name: "CamelCase", input: []byte("NuLl"), _type: Null, value: []byte("NuLl")},
		{name: "spaces", input: []byte("  Null\r\n "), _type: Null, value: []byte("Null")},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			simpleValid(&test, t)
		})
	}
}

func TestUnmarshal_NullSimpleCorrupted(t *testing.T) {
	tests := []testCase{
		{name: "nul", input: []byte("nul")},
		{name: "NILL", input: []byte("NILL")},
		{name: "spaces", input: []byte("Nu ll")},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			simpleInvalid(&test, t)
		})
	}
}

func TestUnmarshal_BoolSimpleSuccess(t *testing.T) {
	tests := []testCase{
		{name: "lower true", input: []byte("true"), _type: Bool, value: []byte("true")},
		{name: "lower false", input: []byte("false"), _type: Bool, value: []byte("false")},
		{name: "upper true", input: []byte("TRUE"), _type: Bool, value: []byte("TRUE")},
		{name: "upper false", input: []byte("FALSE"), _type: Bool, value: []byte("FALSE")},
		{name: "CamelCase true", input: []byte("TrUe"), _type: Bool, value: []byte("TrUe")},
		{name: "CamelCase false", input: []byte("FaLsE"), _type: Bool, value: []byte("FaLsE")},
		{name: "spaces true", input: []byte("  True\r\n "), _type: Bool, value: []byte("True")},
		{name: "spaces false", input: []byte("  False\r\n "), _type: Bool, value: []byte("False")},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			simpleValid(&test, t)
		})
	}
}

func TestUnmarshal_BoolSimpleCorrupted(t *testing.T) {
	tests := []testCase{
		{name: "tru", input: []byte("tru")},
		{name: "fals", input: []byte("fals")},
		{name: "tre", input: []byte("tre")},
		{name: "spaces", input: []byte("fal se")},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			simpleInvalid(&test, t)
		})
	}
}
