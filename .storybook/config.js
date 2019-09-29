import { configure } from '@storybook/react';

const req = require.context('../stories/', true, /Stories\.purs$/);

function loadStories() {
  req.keys().forEach(filename => req(filename).stories());
}

configure(loadStories, module);
