import React from "react";
import PropTypes from "prop-types";
import { graphql, useStaticQuery } from "gatsby";
import { preToCodeBlock } from "mdx-utils";
const psLayout = require("./PSLayout.purs");
const fetch = typeof window !== "undefined" && window.fetch;

const PSLayout = psLayout.mkLayout(fetch)();

const Layout = ({ children }) => {
  const siteInfo = useStaticQuery(graphql`
    query SiteTitleQuery {
      site {
        siteMetadata {
          title
        }
      }
    }
  `);
  return <PSLayout siteInfo={siteInfo}>{children}</PSLayout>;
};

Layout.propTypes = {
  children: PropTypes.node.isRequired
};

export default Layout;
