import { configure, forceReRender } from "@storybook/react";

const localStories = require.context("../src/", true, /Stories\.purs$/);

function loadStories() {
  localStories.keys().forEach(filename => {
    localStories(filename).stories();
  });
}

configure(loadStories, module);
if (module.hot) {
  forceReRender();
}
