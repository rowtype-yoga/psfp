import { configure, forceReRender } from '@storybook/react';
import equal from 'deep-equal';

const req = require.context('../stories/', true, /Stories\.purs$/);

function loadStories() {
  req.keys().forEach(filename => {
    req(filename).stories();
  })
}

configure(loadStories, module);
if (module.hot) {
  forceReRender();
}
