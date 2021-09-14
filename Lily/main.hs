import Exercise1 (_main)
import Exercise3 (_main)
import Exercise4 (_main)

main :: IO ()
main = do
  Exercise1._main
  Exercise3._main
  Exercise4._main

  putStrLn "\n\n✨ All Done! ✨"

