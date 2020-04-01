"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var storybook = require("@storybook/react");
exports.storiesOfImpl = storybook.storiesOf;
exports.addImpl = function (sb) { return function (render) { return function (name) { return function () { return sb.add(name, render); }; }; }; };
exports.addDecoratorImpl = function (sb) { return function (addDeco) { return function () {
    return sb.addDecorator(function (storyFn) { return addDeco(storyFn)(); });
}; }; };
