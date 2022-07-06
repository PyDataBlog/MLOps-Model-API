# Editing a document with Vim

Today was the first time I used Vim to edit a text file. I'd previously install Vim and attempted to play around with it, but I had no clue what I was doing and abandoned it until now.

My issue was that I couldn't figure out how to actually add or edit the contents of a text file. Silly, I know, but it wasn't cut and dry and I didn't have the patience to figure it out at the time.

So, here's what you do:

```
vim [existing file name, or create a new file]
```
This command will launch Vim in the terminal in 'Command mode' with either your existing file or a blank one. If there is already text in the file, you'll be able to see it, but NOT edit it. To do this, you'll need to type `i` to enter insert mode, where you'll have full control of your file. From here, you can type to your heart's content. When you're through, hit `esc` to re-enter command mode. 

From command mode, you can quit AND save your work using `:wq` or just quit by typing `:q`.

This is pretty much the bare minimum needed to use Vim. I'm going to try and add it to my workflow for these TIL posts. It'd be neat to do everything straight form Terminal.

I'll of course be back to update my progress learning Vim. I know it's extremely powerful and that's why I'm excited to learn more!
