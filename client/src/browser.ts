
export function setQuery(query: string) {
  window.history.replaceState({}, "", "?" + query)
}
