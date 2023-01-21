export default {
  logo: <h1 style={{ fontFamily: "monospace" }}>M-x run-command</h1>,
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
  banner: {
    dismissible: false,
    text: (
      <span>
        ⚠️ This documentation refers to the <strong>develop</strong> branch,
        which is not yet released on MELPA.
      </span>
    ),
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
        © 2021-{new Date().getFullYear()}{" "}
        <a
          style={{ textDecoration: "underline" }}
          href="https://massimilianomirra.com/tags/emacs"
          target="_blank"
        >
          Massimiliano Mirra
        </a>
      </span>
    ),
  },
};
