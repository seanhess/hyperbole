Javascript API
-----------------

Requirements

1. Call runAction() - you ought to be able to tell the server to run a particular action, and allow the normal update cycle to happen


### Javascript Components
1. data-xxxx updates
2. trigger events to update - the server controls how to serialize this 



https://github.com/seanhess/hyperbole/issues/25
- wants to be able to trigger an action, and run javascript during it...
- but 




Phoenix LiveView MouseOver / MouseEnter:
----------------------------------------

    import { Socket } from "phoenix";
    import { LiveSocket } from "phoenix_live_view";

    // Register hooks here
    let Hooks = {};
    Hooks.HoverHook = {
      mounted() {
        console.log("Hook mounted!");
        this.el.addEventListener("mouseenter", () => {
          this.pushEvent("mouse_enter", { id: this.el.id });
        });
        this.el.addEventListener("mouseleave", () => {
          this.pushEvent("mouse_leave", { id: this.el.id });
        });
      }
    };

    // Initialize LiveSocket with hooks
    let liveSocket = new LiveSocket("/live", Socket, {
      hooks: Hooks, // Hooks get passed here
    });

    // Connect LiveSocket
    liveSocket.connect();


Phoenix LiveView Push Event to Client
--------------------------------------

    def handle_event("firebase_login", %{"token" => token}, socket) do
      case UserAuth.verify_firebase_token(token) do
        {:ok, user_info} ->
          socket =
            socket
            |> assign(:current_user, user_info)  # Triggers re-render
            |> push_event("auth_success", %{email: user_info.email})  # Sends data to JS

          {:noreply, socket}
      end
    end


    Hooks.FirebaseAuth = {
      mounted() {
        this.handleEvent("auth_success", ({ email }) => {
          console.log("Authenticated as:", email);
          document.getElementById("user-info").innerText = `Logged in as ${email}`;
        });

        this.handleEvent("auth_failed", () => {
          console.log("Authentication failed.");
          document.getElementById("user-info").innerText = "Login failed.";
        });
      }
    };
