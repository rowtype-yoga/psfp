import { configure, forceReRender } from "@storybook/react";

const req = require.context("../src/", true, /Stories\.purs$/);

function loadStories() {
  req.keys().forEach(filename => {
    req(filename).stories();
  });
}

configure(loadStories, module);
if (module.hot) {
  forceReRender();
}
