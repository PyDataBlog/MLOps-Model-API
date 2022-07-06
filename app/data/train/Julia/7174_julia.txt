function main(window)

    push!(window.assets, "widgets")
    push!(window.assets, "layout2")
    push!(window.assets, "vertical-tabs")

    clicks = Signal{Any}(leftbutton)

    e1 = Elem(:svg, :svg, Elem(:svg, :circle, cx=250, cy=250, r=100, fill="orange"), width=500, height=500) |> clickable([leftbutton, rightbutton, scrollbutton])


    vbox(
         subscribe(clicks, e1),
         map(x -> title(1,x), clicks),
         Elem("vertical-tabs")
         )
end
