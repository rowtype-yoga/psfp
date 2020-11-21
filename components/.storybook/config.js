import { configure } from "@storybook/react";

const localStories = require.context("../src/", true, /Stories\.purs$/);

function loadStories() {
  localStories.keys().forEach((filename) => {
    localStories(filename).stories(
      require.cache["./src" + filename.substr(1)]
    )();
  });
}

configure(loadStories, module);

if (module.hot) {
  module.hot.accept(() => configure(loadStories, module));
}
