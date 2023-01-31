export default {
  logo: (
    <h1
      style={{
        fontFamily: "monospace",
        background: "black",
        color: "white",
        padding: "1px 6px",
        borderRadius: "4px",
      }}
    >
      $ M-x run-command
    </h1>
  ),
  project: {
    link: "https://github.com/bard/emacs-run-command",
  },
  docsRepositoryBase:
    "https://github.com/bard/emacs-run-command/tree/master/docs",
  editLink: {
    text: "Edit this page →",
  },
  feedback: {
    content: "Questions? Send feedback →",
    labels: "feedback",
  },
  toc: {
    float: true,
  },
  useNextSeoProps() {
    return {
      titleTemplate: "%s – emacs-run-command",
    };
  },
  gitTimestamp: null,
  head: (
    <>
      <meta name="msapplication-TileColor" content="#fff" />
      <meta httpEquiv="Content-Language" content="en" />
      <meta
        name="description"
        content="Efficient and ergonomic external command invocation for Emacs"
      />
      <meta name="twitter:card" content="summary_large_image" />
      <meta property="og:title" content="emacs-run-command" />
      <meta name="apple-mobile-web-app-title" content="emacs-run-command" />
      <meta property="og:title" content="emacs-run-command" />
      <meta
        property="og:description"
        content="Efficient and ergonomic external command invocation for Emacs"
      />
    </>
  ),
  footer: {
    text: (
      <span>
        © 2020-{new Date().getFullYear()}{" "}
        <a
          style={{ textDecoration: "underline" }}
          href="https://massimilianomirra.com/"
          target="_blank"
        >
          Massimiliano Mirra
        </a>
      </span>
    ),
  },
};
