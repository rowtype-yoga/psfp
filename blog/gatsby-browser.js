import React from "react";
import {JssProvider, SheetsRegistry} from 'react-jss'

export const onInitialClientRender = () => {
  if (process.env.BUILD_STAGE === `develop`) {
    return;
  }

  // Remove the server-side injected CSS.
  const jssStyles = document.querySelector(`#jss-server-side`);
  if (jssStyles) {
    jssStyles.parentNode.removeChild(jssStyles);
  }
};

export const wrapRootElement = ({ element }) => {

  return element;
//   return <StylesProvider {...stylesProvider}>{element}</StylesProvider>;
//   return <JssProvider registry={sheets}>{element}</JssProvider>
};