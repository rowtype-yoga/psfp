import storybook = require("@storybook/react");

exports.storiesOfImpl = storybook.storiesOf;

exports.addImpl = sb => render => name => () => sb.add(name, render);

exports.addDecoratorImpl = sb => addDeco => () =>
  sb.addDecorator(storyFn => addDeco(storyFn)());
