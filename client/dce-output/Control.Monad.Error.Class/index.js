// Generated by purs version 0.13.6
"use strict";
var Control_Applicative = require("../Control.Applicative/index.js");
var Data_Either = require("../Data.Either/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var MonadThrow = function (Monad0, throwError) {
    this.Monad0 = Monad0;
    this.throwError = throwError;
};
var MonadError = function (MonadThrow0, catchError) {
    this.MonadThrow0 = MonadThrow0;
    this.catchError = catchError;
};
var throwError = function (dict) {
    return dict.throwError;
};
var catchError = function (dict) {
    return dict.catchError;
};
var $$try = function (dictMonadError) {
    return function (a) {
        return catchError(dictMonadError)(Data_Functor.map(((((dictMonadError.MonadThrow0()).Monad0()).Bind1()).Apply0()).Functor0())(Data_Either.Right.create)(a))((function () {
            var $80 = Control_Applicative.pure(((dictMonadError.MonadThrow0()).Monad0()).Applicative0());
            return function ($81) {
                return $80(Data_Either.Left.create($81));
            };
        })());
    };
};
module.exports = {
    catchError: catchError,
    throwError: throwError,
    MonadThrow: MonadThrow,
    MonadError: MonadError,
    "try": $$try
};