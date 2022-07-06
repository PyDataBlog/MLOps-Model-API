import Escher: @api, render

desc = md"""
# Welcome fellow scientist!
This is **Mjollnir** a tool intended to simplify working with data.
""" |> Escher.pad(1em)

main(window) = begin
    push!(window.assets, "layout2")
    push!(window.assets, "icons")
    push!(window.assets, "widgets")

    t, p = wire(tabs([hbox(icon("home"), hskip(1em), "Home"),
                      hbox(icon("info-outline"), hskip(1em),  "Notifications"),
                      hbox(icon("settings"), hskip(1em), "Settings")]),
                pages([desc, "Notification tab content", "Settings tab content"]), :tabschannel, :selected)

    vbox(toolbar([iconbutton("face"), "Mjollnir", flex(), iconbutton("search")]),
         maxwidth(30em, t),
         Escher.pad(1em, p))
end
