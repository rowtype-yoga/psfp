import { main } from "./output/Main/index.js"
if(main === undefined) {
    console.error("You are missing a 'main' function")
} else if (main instanceof Function) {
  main()
}
else {
  console.error("Make sure that your main function is of type `Effect Unit`")
}
