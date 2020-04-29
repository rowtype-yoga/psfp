const path = require("path");
var proxy = require("http-proxy-middleware");

module.exports = {
  siteMetadata: {
    title: "Rowtype Yoga",
    description: "Rowtype Yoga",
    author: "Mark Eibes",
    menuLinks: [{ name: "home", link: "/" }]
  },
  developMiddleware: app => {
    app.use(
      "/api",
      proxy({
        target: "http://127.0.0.1:14188",
        pathRewrite: {
          "/api": ""
        }
      })
    );
  },
  plugins: [
    "gatsby-plugin-purescript",
    { resolve: "gatsby-plugin-react-svg" },
    {
      resolve: `gatsby-plugin-mdx`,
      options: {
        defaultLayouts: { default: path.resolve("./src/components/layout.js") }
      }
    },
    {
      resolve: `gatsby-plugin-alias-imports`,
      options: {
        alias: {
          react: path.resolve("./node_modules/react"),
          "react-dom": path.resolve("./node_modules/react-dom")
        },
        extensions: []
      }
    },
    "gatsby-plugin-react-helmet",
    {
      resolve: `gatsby-source-filesystem`,
      options: {
        name: `images`,
        path: `${__dirname}/src/images`
      }
    },
    {
      resolve: `gatsby-source-filesystem`,
      options: {
        name: `pages`,
        path: `${__dirname}/src/pages`
      }
    },
    "gatsby-transformer-sharp",
    "gatsby-plugin-sharp",
    {
      resolve: `gatsby-plugin-manifest`,
      options: {
        name: "gatsby-default-mdx-basic",
        short_name: "starter",
        start_url: "/",
        background_color: "#663399",
        theme_color: "#663399",
        display: "minimal-ui",
        icon: "src/images/logo.png" // This path is relative to the root of the site.
      }
    }
    // this (optional) plugin enables Progressive Web App + Offline functionality
    // To learn more, visit: https://gatsby.app/offline
    // 'gatsby-plugin-offline',
  ]
};
