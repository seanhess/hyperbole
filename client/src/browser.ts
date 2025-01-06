
export function setQuery(query: string = "") {
  console.log("setQuery", query)
  if (query) {
    window.history.replaceState({}, "", location.pathname + "?" + query)
  }
  else {
    window.history.replaceState({}, "", location.pathname)
  }
}
