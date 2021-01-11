import React from "react";
import { graphql, useStaticQuery } from "gatsby";
// Load fonts
require("../../static/fonts/fonts")
const psLayout = require("../../output/PSLayout")
const mdxProvider = require("../../output/MDXProvider")
const fetch = typeof window !== "undefined" && window.fetch;


const PSLayout = psLayout.mkLayout(fetch)();
const MdxProvider = mdxProvider.mkLiveMdxProviderComponent(fetch)()

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
    <PSLayout siteInfo={q.site} mdxProviderComponent={MdxProvider}>
      {children}
    </PSLayout>
  );
};

export default Layout;
