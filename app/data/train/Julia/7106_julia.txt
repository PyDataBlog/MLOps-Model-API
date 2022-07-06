import Escher: @api, render

####  main function  ####
function main(window)

    ###  pushing window assets ###
    push!(window.assets, "layout2")
    push!(window.assets, "icons")
    push!(window.assets, "widgets")
    push!(window.assets, "codemirror")
    
    ### making a tab bar: step 1 -creating pages content  ###
    page1 = md"""
                Welcome fellow scientist!
                This is **Mjolnir** -a tool intended to simplify working with data
            """
    
    page2 = Elem(:svg, :svg, width="100", height="100", Elem(:svg, :circle, cx="50", cy="50", r="40", stroke="green", strokeWidth="4", fill="yellow"))

    page3 = vbox(
                 Elem(:svg, :svg, width="100", height="100", Elem(:svg, :circle, cx="50", cy="50", r="40", stroke="green", strokeWidth="4", fill="yellow")),
                 plaintext("tab3")
                 )

    ###  making a tab bar: step 2 -wiring tabs and pages ###
    t, p = wire(tabs([hbox(icon("home"), hskip(1em), "project description"),
                      hbox(icon("info-outline"), hskip(1em),  "methodology"),
                      hbox(icon("settings"), hskip(1em), "settings")]),
                pages([page1, page2, page3]), :tabschannel, :selected)

    ###  creating a code-mirror element
    inp = Signal(Dict())
    s = sampler()

    editor = watch!(s, :code, codemirror())
    code_cell = trigger!(s, :submit, keypress("ctrl+enter shift+enter", editor))


    ###  structuring and displaying content  ###
    vbox(
         toolbar([iconbutton("face"), "Mjollnir", flex(), iconbutton("search")]),
         hbox(
              vbox(
                   maxwidth(30em, t),
                   Escher.pad(2em, p)
                   ),
              
              hskip(3em),
              
              vbox(
                   md"""code""",
                   intent(s, code_cell) >>> inp,
                   inp
                   )
         )
    )
    
end
