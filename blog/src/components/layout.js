import React from "react";
import PropTypes from "prop-types";
import { StaticQuery, graphql } from "gatsby";
import { MDXProvider } from "@mdx-js/react";
import { preToCodeBlock } from "mdx-utils";
const yogaCard = require("./YogaCard.purs");

import Header from "./header";
import Code from "./code";

const mdxProviderComponents = {
  pre: preProps => {
    const props = preToCodeBlock(preProps);
    return props ? <Code {...props} /> : <pre {...preProps} />;
  }
};

const ThemeProvider = yogaCard.makeThemeProvider();

const Layout = ({ children }) => (
  <ThemeProvider theme={yogaCard.dark}>
    <MDXProvider components={mdxProviderComponents}>
      <StaticQuery
        query={graphql`
          query SiteTitleQuery {
            site {
              siteMetadata {
                title
              }
            }
          }
        `}
        render={data => (
          <div style={{backgroundColor: yogaCard.dark.backgroundColour}}>
            <Header siteTitle={data.site.siteMetadata.title} />
            <div>{children}</div>
          </div>
        )}
      />
    </MDXProvider>
  </ThemeProvider>
);

Layout.propTypes = {
  children: PropTypes.node.isRequired
};

export default Layout;
