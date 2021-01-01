const React = require("react")
const createElement = require('@mdx-js/react').mdx

exports.mdxImpl = (mdxCode) => mdx.default(mdxCode, { skipExport: true })

exports.renderWithReact = code => {
    const fn = new Function(
        'React',
        'mdx',
        `${code}; return React.createElement(MDXContent)`
    )
    return fn(React, createElement)
}