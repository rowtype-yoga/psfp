import React from 'react'
import {JssProvider, SheetsRegistry} from 'react-jss'

const globalLeak = new Map();

export const wrapRootElement = ({ element, pathname }) => {
  const sheets = new SheetsRegistry()
  globalLeak.set(pathname, sheets);

  return <JssProvider registry={sheets}>{element}</JssProvider>
}

export const onRenderBody = (
    { setHeadComponents, pathname },
  ) => {
    const sheets = globalLeak.get(pathname);
  
    if (!sheets) {
      return;
    }
  
    const css = sheets.toString();
  
    // css = disableAutoprefixing ? css : autoprefixer(css, pathname);
    // css = disableMinification ? css : cleanCSS.minify(css).styles;
  
    setHeadComponents([
      <style
        id="jss-server-side"
        key="jss-server-side"
        dangerouslySetInnerHTML={{ __html: css }}
      />,
    ]);
  
    globalLeak.delete(pathname);
  };

