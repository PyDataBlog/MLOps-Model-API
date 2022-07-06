lyrics = Array[
  "My dick cost a late night fee, your dick got the HIV",
  "My dick plays on the double feature screen, your dick went straight to DVD",
  "My dick - bigger than a bridge, your dick look like a little kid's",
  "My dick - large like the Chargers, the whole team, your shit look like you fourteen",
  "My dick - locked in a cage, right, your dick suffer from stage fright",
  "My dick - so hot, it's stolen, your dick look like Gary Coleman",
  "My dick - pink and big, your dick stinks like shit",
  "My dick got a Caesar do, your dick needs a tweezer, dude",
  "My dick is like super size, your dick look like two fries",
  "My dick - more mass than the Earth, your dick - half staff, it needs work",
  "My dick - been there done that, your dick sits there with dunce cap",
  "My dick - VIP, Your shit needs ID",
  "My dick need no introduction, Your dick don't even function",
  "My dick served a whole luncheon, your dick - it look like a munchkin",
  "My dick - size of a pumpkin, your dick look like Macaulay Culkin",
  "My dick - good good lovin', your dick - good for nothin'",
  "My dick bench pressed 350, your dick couldn't shoplift at Thrifty",
  "My dick - pretty damn skippy, your dick - hungry as a hippie",
  "My dick don't fit down the chimney, your dick is like a kid from the Philippines",
  "My dick is like an M16, your dick - broken vending machine",
  "My dick parts the seas, your dick farts and queefs",
  "My dick - rumble in the jungle, your dick got touched by your uncle",
  "My dick goes to yoga, your dick - fruit roll-up",
  "My dick - grade-A beef, your dick - Mayday geek",
  "My dick - sick and dangerous, your dick - quick and painless",
  "My dick - 'nuff said, your dick loves Fred"
]


Z3PO.message(contains: /^(penis|dick)$/i) do |event|
  Z3PO.send_message(event.channel, "<@#{event.user.id}>, #{lyrics.sample}")
end
