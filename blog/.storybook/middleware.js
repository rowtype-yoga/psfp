const express = require("express");

const babel = require('@babel/core')
const React = require('react')
const { renderToStaticMarkup } = require('react-dom/server')
const mdx = require('@mdx-js/mdx')
const { MDXProvider, mdx: createElement } = require('@mdx-js/react')

const expressMiddleWare = router => {
    router.use(express.text())
    router.use("/mdx", async (req, res) => {
        const rendered = await renderWithReact(req.body)
        res.send(rendered)
    });
};

const transform = code =>
    babel.transform(code, {
        plugins: [
            '@babel/plugin-transform-react-jsx',
            '@babel/plugin-proposal-object-rest-spread'
        ]
    }).code

const renderWithReact = async mdxCode => {
    const jsx = await mdx(mdxCode, { skipExport: true })
    const code = transform(jsx)
    const fn = new Function(
        'React',
        'mdx',
        `${code}; return React.createElement(MDXContent)`
    )
    const element = fn(React, createElement)
    const components = {
        h1: ({ children }) =>
            React.createElement('h1', { style: { color: 'tomato' } }, children)
    }
    const elementWithProvider = React.createElement(
        MDXProvider,
        { components },
        element
    )
    const result = renderToStaticMarkup(elementWithProvider)
    return code
}

module.exports = expressMiddleWare;