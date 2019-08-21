import * as mobx from "mobx";
import * as React from "react";
import * as ReactDOM from "react-dom";

import { ADL } from "./adl-gen/resolver";
import { MARKDOWN_DOCS } from "./markdown-docs";
import { Router } from "./routing/router";
import { App } from "./ui/app";
import { AdlStore } from "./util/adl-utils";

mobx.configure({
  // Enforce state changes to occur with a method decorated by @action or runInAction
  enforceActions: "observed",
});

const adlStore = new AdlStore(
  ADL,
  [
    { title: "Requests", requests: (r) => true },
  ],
  [
  ],
);

// If true, show the ADL definition as well as the openapi
const showDeclAdl = false;

const router = new Router(location.pathname, window.history, window);

const app = (
  <App
    origin={window.location.origin}
    router={router}
    adlStore={adlStore}
    docs={MARKDOWN_DOCS}
    showDeclAdl={showDeclAdl}
  />
);

ReactDOM.render(app, document.getElementById("root") as HTMLElement);
