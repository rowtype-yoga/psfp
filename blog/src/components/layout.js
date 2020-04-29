import React from "react";
import PropTypes from "prop-types";
import { graphql, useStaticQuery } from "gatsby";
import { preToCodeBlock } from "mdx-utils";
const psLayout = require("../../output/PSLayout/index.js"); // [FIXME] use the next line
// const psLayout = require("./PSLayout.purs");
const fetch = typeof window !== "undefined" && window.fetch;

const PSLayout = psLayout.mkLayout(fetch)();

const Layout = ({ children }) => {
  const siteInfo = useStaticQuery(graphql`
    query SiteTitleQuery {
      site {
        siteMetadata {
          title
          menuLinks {
            name
            link
          }
        }
      }
    }
  `);
  return <PSLayout siteInfo={siteInfo}>{children}</PSLayout>;
};

export default Layout;