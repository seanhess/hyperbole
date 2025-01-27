
export function setQuery(query: string = "") {
  if (query != currentQuery()) {
    if (query != "") query = "?" + query
    let url = location.pathname + query
    console.log("history.replaceState(", url, ")")
    window.history.replaceState({}, "", url)
  }
}

function currentQuery(): string {
  const query = window.location.search;
  return query.startsWith('?') ? query.substring(1) : query;
}
