function Base.display(plot::Plot)
    @assert length(plot.layers) > 0

    # compute ranges
    xmin, xmax = Inf, -Inf
    ymin, ymax = Inf, -Inf
    for layer in plot.layers
        xmin = min(xmin, x_minimum(layer))
        xmax = max(xmax, x_maximum(layer))
        ymin = min(ymin, y_minimum(layer))
        ymax = max(ymax, y_maximum(layer))
    end

    # adjust ranges
    xmin, xmax = adjustrange(xmin, xmax)
    ymin, ymax = adjustrange(ymin, ymax)

    # set window
    viewport = (0.13, 0.95, 0.10, 0.95)
    setviewport(viewport...)
    setcharheight(0.020)
    settextcolorind(82)
    clearws()
    setwindow(xmin, xmax, ymin, ymax)

    # grid and axes
    xmajor = ymajor = 5
    xtick = tick(xmin, xmax) / xmajor
    ytick = tick(ymin, ymax) / ymajor
    grid(xtick, ytick, xmax, ymax, xmajor, ymajor)
    ticksize = 0.0125 * (viewport[2] - viewport[1])
    axes(xtick, ytick, xmin, ymin,  xmajor,  ymajor,  ticksize)
    axes(xtick, ytick, xmax, ymax, -xmajor, -ymajor, -ticksize)
    text(0.50, 0.03, string(plot.aes.x))
    setcharup(-1, 0)
    text(0.03, 0.50, string(plot.aes.y))

    # plot layers
    for layer in plot.layers
        plotlayer(layer)
    end
    updatews()

    return nothing
end
