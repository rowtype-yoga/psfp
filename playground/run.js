const main = require ("./output/Main")
if(main.main == undefined) {
    console.error("You are missing a 'main' function")
} else if (main.main instanceof Function) {
  main.main()
}
else {
  console.error("Make sure that you're main function is of type `Effect Unit`")
}
