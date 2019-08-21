/** Listener on route changes */
export type RouteChangeListener = () => void;

/** Subset of the pop event that we need */
type PopEventSubset = Pick<PopStateEvent, "state">;

/** Subset of location that we need */
type LocationSubset = Pick<Location, "pathname">;

/** Subset of the window interface that we need */
interface WindowSubset {
  onpopstate: ((ev: PopEventSubset) => void) | null;
  location: Pick<Location, "pathname">;
}

/**
 * Abstraction over browser history
 *
 * Router does not depend on mobx. The expected pattern is each app has a
 * navigator store, which will register a routeChangeListener, parsing
 * route state into type-safe app specific state.
 */
export class Router {
  /** Listeners on route changes */
  private readonly routeChangeListeners: RouteChangeListener[] = [];

  /** The current path (as displayed in the browser) */
  private currentPath: string;

  /** create a new router, with a given starting path and URL history management interface */
  constructor(
    initialPath: string,
    /** Subset of the history API we keep in sync with the router model */
    private readonly history: Pick<History, "pushState" | "replaceState" | "back">,
    /** Subset of the window interface */
    window: WindowSubset,
  ) {
    this.currentPath = initialPath;

    // Decorate the current window.popstate listener with our own, which ensures currentPath is updated */
    const originalHandler = window.onpopstate;
    window.onpopstate = (ev: PopEventSubset) => {
      console.log("onpopstate:", ev);
      this.setCurrentPath(window.location.pathname);
      if (originalHandler) {
        originalHandler.apply(window, [ev]);
      }
    };
  }

  /** Create a router suitable for use in tests (no dependecy on browser 'window' object) */
  public static createTestFake(initialPath?: string): Router {
    const paths: string[] = [];
    if (initialPath) {
      paths.push(initialPath);
    } else {
      paths.push("");
    }

    function getLatestLocation(): LocationSubset {
      return {
        pathname: paths[paths.length - 1],
      };
    }

    const dummyWindow: WindowSubset = {
      onpopstate: () => undefined,
      location: getLatestLocation(),
    };

    return new Router(
      getLatestLocation().pathname,
      {
        pushState: (data, title, path) => {
          if (typeof(path) === "string") {
            paths.push(path);
          }
          console.log("push, paths:", paths);
        },
        replaceState: (data, title, path) => {
          if (typeof(path) === "string") {
            paths[paths.length - 1] = path;
          }
          console.log("replace, paths:", paths);
        },
        back: () => {
          console.log("back, paths:", paths);
          paths.pop();
          dummyWindow.location = getLatestLocation();
          if (dummyWindow.onpopstate) {
            dummyWindow.onpopstate({ state: {} });
          }
        },
      },
      dummyWindow,
    );
  }

  /** Registers a listener on route changes and calls it immediately */
  registerRouteChangeListener(onRouteChange: RouteChangeListener) {
    this.routeChangeListeners.push(onRouteChange);
    onRouteChange();
  }

  /** Change the current page path, and optionally store some associated state */
  go(path: string, state?: {}) {
    setTimeout(() => {
      this.history.pushState(state || {}, "", path);
      this.setCurrentPath(path);
    });
  }

  /** Replace the current page path, and optionally store some associated state */
  replace(path: string, state?: {}) {
    setTimeout(() => {
      this.history.replaceState(state || {}, "", path);
      this.setCurrentPath(path);
    });
  }

  /** Goes back in history */
  back() {
    setTimeout(() => {
      this.history.back();
    });
  }

  /**
   * Check if the current patch matches the provided pattern.
   *
   * The regex provided must match completely, for example if the path is '/a/b/c'
   * a matches('a') will fail, however matches('a.*') will succeed
   *
   * @param pattern
   */
  matches(pattern: string): boolean {
    return this.currentPath.match(`^${pattern}$`) !== null;
  }

  /**
   * Return the current path, split on '/', URL decoded.
   *
   * Useful to pull out path parts,
   * e.g. if the path is /user/:id then:
   * `const [_0, id] = courter.getPathParts()`
   * to get the relevant bits.
   */
  getPathParts(): string[] {
    let parts = this.currentPath.split("/");
    if (this.currentPath[0] === "/") {
      parts = parts.slice(1);
    }
    return parts.map(decodeURIComponent);
  }

  /** Sets the current path and notifies listeners */
  private setCurrentPath(path: string) {
    console.log(`navigating to: ${path}`);
    this.currentPath = path;
    for (const listener of this.routeChangeListeners) {
      listener();
    }
  }
}
