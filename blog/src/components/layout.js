import React from "react";
import PropTypes from "prop-types";
import { graphql, useStaticQuery } from "gatsby";
import { preToCodeBlock } from "mdx-utils";
const psLayout = require("../../output/PSLayout/index.js"); // [FIXME] use the next line
// const psLayout = require("./PSLayout.purs");
const fetch = typeof window !== "undefined" && window.fetch;
import Img from "gatsby-image";

const PSLayout = psLayout.mkLayout(fetch)();

const query = graphql`
  query {
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
`;

const Layout = ({ children }) => {
  const q = useStaticQuery(query);
  return (
    <PSLayout siteInfo={q.site}>
      {children}
    </PSLayout>
  );
};

export default Layout;
