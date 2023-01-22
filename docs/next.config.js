const remarkSmartypants = require("@silvenon/remark-smartypants");
const withNextra = require("nextra")({
  theme: "nextra-theme-docs",
  themeConfig: "./theme.config.jsx",
  mdxOptions: {
    remarkPlugins: [remarkSmartypants],
  },
});

let assetPrefix, basePath;
if (process.env.GITHUB_ACTIONS) {
  const repo = process.env.GITHUB_REPOSITORY.replace(/.*?\//, "");
  assetPrefix = `/${repo}/`;
  basePath = `/${repo}`;
} else {
  assetPrefix = undefined;
  basePath = "";
}

module.exports = withNextra({
  assetPrefix,
  basePath,
  images: {
    unoptimized: true,
  },
});
