import React from "react";
import PropTypes from "prop-types";
import { graphql, useStaticQuery } from "gatsby";
import { preToCodeBlock } from "mdx-utils";
const psLayout = require("../../output/PSLayout"); // [FIXME] use the next line
const fetch = typeof window !== "undefined" && window.fetch;
import Img from "gatsby-image";

const PSLayout = psLayout.mkLayout(fetch)();
const MdxProvider = psLayout.mkLiveMdxProviderComponent(fetch)()

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
    <PSLayout siteInfo={q.site} mdxProviderComponent={mdxProvider}>
      {children}
    </PSLayout>
  );
};

export default Layout;
