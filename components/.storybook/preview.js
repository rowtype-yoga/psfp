const whyDidYouRender = require('@welldone-software/why-did-you-render');
const React = require("react")
whyDidYouRender(React, {
  trackAllPureComponents: true,
});

export const parameters = {
  actions: { argTypesRegex: "^on[A-Z].*" },
}