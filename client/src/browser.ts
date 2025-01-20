
export function setQuery(query: string = "") {

  if (location.search != query) {
    if (query != "") query = "?" + query
    let url = location.pathname + query
    console.log("history.replaceState(", url, ")")
    window.history.replaceState({}, "", url)
  }
}
