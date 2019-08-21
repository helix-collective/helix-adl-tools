import { ScopedName } from "../adl-gen/runtime/sys/adlast";
import { Router } from "./router";

export type Route
  = { root: "api_summary" }
  | { root: "openapi" }
  | { root: "docs", page: string }
  | { root: "decl", scopedName: ScopedName }
  | { root: "request", scopedName: ScopedName }
  ;

export function getRoute(router: Router): Route | undefined {
  return routeFromParts(router.getPathParts());
}

export function setRoute(router: Router, route: Route) {
  router.go(pathFromRoute(route));
}

export function pathFromRoute(route: Route): string {
  return "/" + partsFromRoute(route).join("/");
}

function routeFromParts(parts: string[]): Route | undefined {
  if (parts.length == 1 && parts[0] === "api_summary") {
    return { root: "api_summary" };
  } else if (parts.length == 1 && parts[0] === "openapi") {
    return { root: "openapi" };
  } else if (parts.length == 2 && parts[0] === "docs") {
    return { root: "docs", page: parts[1] };
  } else if (parts.length == 3 && parts[0] === "decl") {
    const scopedName = {
      moduleName: parts[1],
      name: parts[2],
    };
    return { root: "decl", scopedName };
  } else if (parts.length == 3 && parts[0] === "request") {
    const scopedName = {
      moduleName: parts[1],
      name: parts[2],
    };
    return { root: "request", scopedName };
  } else {
    return undefined;
  }
}

function partsFromRoute(route: Route): string[] {
  const parts = [];
  parts.push(route.root);
  switch (route.root) {
    case "api_summary":
    case "openapi":
      break;
    case "docs":
      parts.push(route.page);
      break;
    case "decl":
      parts.push(route.scopedName.moduleName);
      parts.push(route.scopedName.name);
      break;
    case "request":
      parts.push(route.scopedName.moduleName);
      parts.push(route.scopedName.name);
  }
  return parts;
}
