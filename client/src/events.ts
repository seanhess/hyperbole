
export type UrlFragment = string

export function listenClick(cb:(target:HTMLElement, action:string) => void): void {
  document.addEventListener("click", function(e) {
    let el = e.target as HTMLInputElement

    // clicks can fire on internal elements. Find the parent with a click handler
    let source = el.closest("[data-on-click]") as HTMLElement

    // console.log("CLICK", source?.dataset.onClick)

    // they should all have an action and target
    if (source?.dataset.onClick && source?.dataset.target) {
      e.preventDefault()

      let target = document.getElementById(source.dataset.target)

      if (!target) {
        console.error("Missing target: ", source.dataset.target)
        return
      }

      cb(target, source.dataset.onClick)
    }
  })
}


export function listenChange(cb:(target:HTMLElement, action:string) => void): void {
  document.addEventListener("change", function(e) {
    let el = e.target as HTMLElement

    // clicks can fire on internal elements. Find the parent with a click handler
    let source = el.closest("[data-on-change]") as HTMLInputElement

    // console.log("CHANGE!", source.value)

    // they should all have an action and target
    if (source?.dataset.target && source.value) {
      e.preventDefault()

      let target = document.getElementById(source.dataset.target)

      if (!target) {
        console.error("Missing target: ", source.dataset.target)
        return
      }

      cb(target, source.value)
    }
  })
}

export function listenFormSubmit(cb:(target:HTMLElement, action:string, form:FormData) => void): void {
  document.addEventListener("submit", function(e) {
    let form = e.target as HTMLFormElement

    // they should all have an action and target
    if (form?.dataset.onSubmit && form?.dataset.target) {
      e.preventDefault()

      let target = document.getElementById(form.dataset.target)

      if (!target) {
        console.error("Missing target: ", form.dataset.target)
        return
      }

      const formData = new FormData(form)
      cb(target, form.dataset.onSubmit, formData)
    }
  })
}
