// Generated by purs version 0.13.6
"use strict";
var $foreign = require("./foreign.js");
var Data_Bounded = require("../Data.Bounded/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Cardinality = function (x) {
    return x;
};
var Enum = function (Ord0, pred, succ) {
    this.Ord0 = Ord0;
    this.pred = pred;
    this.succ = succ;
};
var BoundedEnum = function (Bounded0, Enum1, cardinality, fromEnum, toEnum) {
    this.Bounded0 = Bounded0;
    this.Enum1 = Enum1;
    this.cardinality = cardinality;
    this.fromEnum = fromEnum;
    this.toEnum = toEnum;
};
var toEnum = function (dict) {
    return dict.toEnum;
};
var fromEnum = function (dict) {
    return dict.fromEnum;
};
var toEnumWithDefaults = function (dictBoundedEnum) {
    return function (low) {
        return function (high) {
            return function (x) {
                var v = toEnum(dictBoundedEnum)(x);
                if (v instanceof Data_Maybe.Just) {
                    return v.value0;
                };
                if (v instanceof Data_Maybe.Nothing) {
                    var $134 = x < fromEnum(dictBoundedEnum)(Data_Bounded.bottom(dictBoundedEnum.Bounded0()));
                    if ($134) {
                        return low;
                    };
                    return high;
                };
                throw new Error("Failed pattern match at Data.Enum (line 158, column 33 - line 160, column 62): " + [ v.constructor.name ]);
            };
        };
    };
};
var defaultSucc = function (toEnum$prime) {
    return function (fromEnum$prime) {
        return function (a) {
            return toEnum$prime(fromEnum$prime(a) + 1 | 0);
        };
    };
};
var defaultPred = function (toEnum$prime) {
    return function (fromEnum$prime) {
        return function (a) {
            return toEnum$prime(fromEnum$prime(a) - 1 | 0);
        };
    };
};
var charToEnum = function (v) {
    if (v >= Data_Bounded.bottom(Data_Bounded.boundedInt) && v <= Data_Bounded.top(Data_Bounded.boundedInt)) {
        return new Data_Maybe.Just($foreign.fromCharCode(v));
    };
    return Data_Maybe.Nothing.value;
};
var enumChar = new Enum(function ($dollar__unused) {
    return Data_Ord.ordChar;
}, defaultPred(charToEnum)($foreign.toCharCode), defaultSucc(charToEnum)($foreign.toCharCode));
var boundedEnumChar = new BoundedEnum(function ($dollar__unused) {
    return Data_Bounded.boundedChar;
}, function ($dollar__unused) {
    return enumChar;
}, $foreign.toCharCode(Data_Bounded.top(Data_Bounded.boundedChar)) - $foreign.toCharCode(Data_Bounded.bottom(Data_Bounded.boundedChar)) | 0, $foreign.toCharCode, charToEnum);
module.exports = {
    Enum: Enum,
    BoundedEnum: BoundedEnum,
    toEnum: toEnum,
    fromEnum: fromEnum,
    toEnumWithDefaults: toEnumWithDefaults,
    Cardinality: Cardinality,
    defaultSucc: defaultSucc,
    defaultPred: defaultPred,
    enumChar: enumChar,
    boundedEnumChar: boundedEnumChar
};