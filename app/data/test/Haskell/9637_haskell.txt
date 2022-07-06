--
import Control.Monad
import Control.Exception
import System.Directory (doesFileExist)
import System.Environment
import System.Exit
import qualified Data.Text as T
import qualified Data.List as List

--
iff :: Bool -> a -> a -> a
iff True  x _ = x
iff False _ y = y

--
strings_join :: String -> [String] -> String
strings_join delim [] = ""
strings_join delim (x:[]) = x
strings_join delim (x:xs) = x ++ delim ++ (strings_join delim xs)

--
texts_uniq :: [T.Text] -> [T.Text]
texts_uniq [] = []
texts_uniq (x:xs) =
    let xs_uniq = (texts_uniq xs)
    in
    iff (x `elem` xs_uniq)
        (xs_uniq)
        ([x] ++ xs_uniq)

--
getEnvOrEmpty :: String -> IO String
getEnvOrEmpty name =
    getEnv name `catch`
        -- "(e :: IOException)" is for hinting exception type
        (\e -> let _ = (e :: IOException) in return "")

--
find_exe_paths :: String -> IO [String]
find_exe_paths prog =
    do
        -- 8f1kRCu
        env_pathext <- getEnvOrEmpty "PATHEXT"

        -- 4fpQ2RB
        iff (env_pathext == "")
            -- then
            -- 9dqlPRg
            (return [])
            -- else
            (do
                -- 6qhHTHF
                -- Split into a list of extensions
                let ext_s = T.splitOn (T.pack ";") (T.pack env_pathext)

                -- 2pGJrMW
                -- Strip
                let ext_s_2 = map T.strip ext_s

                -- 2gqeHHl
                -- Remove empty
                let ext_s_3 = filter (\x -> x /= (T.pack "")) ext_s_2

                -- 2zdGM8W
                -- Convert to lowercase
                let ext_s_4 = map T.toLower ext_s_3

                -- 2fT8aRB
                -- Uniquify
                let ext_s_5 = texts_uniq ext_s_4

                -- 4ysaQVN
                env_path <- getEnvOrEmpty "PATH"

                -- 5gGwKZL
                let dir_path_s = iff (env_path == "")
                                    -- then
                                    -- 7bVmOKe
                                    -- Go ahead with "dir_path_s" being empty
                                    []
                                    -- else
                                    -- 6mPI0lg
                                    -- Split into a list of paths
                                    (T.splitOn (T.pack ";") (T.pack env_path))

                -- 5rT49zI
                -- Insert empty dir path to the beginning.
                --
                -- Empty dir handles the case that "prog" is a path, either
                -- relative or absolute. See code 7rO7NIN.
                let dir_path_s2 = [T.pack ""] ++ dir_path_s

                -- 2klTv20
                -- Uniquify
                let dir_path_s3 = texts_uniq dir_path_s2

                -- 9gTU1rI
                -- Check if "prog" ends with one of the file extension in
                -- "ext_s_5".
                --
                -- "ext_s_5" are all in lowercase, ensured at 2zdGM8W.
                let prog_lc = T.toLower (T.pack prog)

                let prog_has_ext = any (`T.isSuffixOf` prog_lc) ext_s_5

                -- 6bFwhbv
                exe_path_s <- liftM List.concat (
                    (`mapM` dir_path_s3) (\dir_path -> do
                        -- 7rO7NIN
                        -- Synthesize a path
                        let path = iff (dir_path == T.pack "")
                                        (T.pack prog)
                                        (T.concat [
                                                    dir_path
                                                    ,(T.pack "\\")
                                                    ,(T.pack prog)
                                                    ])

                        -- "exe_path" is used at 4bm0d25.
                        -- Its value being empty string means file not exist.
                        exe_path <-
                            -- 6kZa5cq
                            -- If "prog" ends with executable file extension
                            iff prog_has_ext
                            -- then
                            (do
                                file_exists <- (doesFileExist (T.unpack path))

                                -- 3whKebE
                                iff file_exists
                                    -- then
                                    -- 2ffmxRF
                                    (return path)
                                    -- else
                                    (return (T.pack ""))
                            )
                            -- else
                            (return (T.pack ""))

                        -- 2sJhhEV
                        -- Assume user has omitted the file extension
                        exe_path_s <- liftM List.concat (
                            (`mapM` ext_s_5) (\ext -> do
                                -- 6k9X6GP
                                -- Synthesize a path with one of the file
                                -- extensions in PATHEXT
                                let path_2 = (T.concat [path, ext])

                                file_exists_2 <-
                                    (doesFileExist (T.unpack path_2))

                                -- 6kabzQg
                                iff file_exists_2
                                    -- then
                                    -- 7dui4cD
                                    (return [path_2])
                                    -- else
                                    (return [])
                                )
                            )

                        -- 4bm0d25
                        iff (exe_path == (T.pack ""))
                            -- then
                            (return exe_path_s)
                            -- then
                            (return ([exe_path] ++ exe_path_s))

                        --
                        )
                    )

                -- 8swW6Av
                -- Uniquify
                let exe_path_s2 = texts_uniq exe_path_s

                -- Convert from Text to String
                let exe_path_s3 = map T.unpack exe_path_s2

                -- 7y3JlnS
                return exe_path_s3
            )

-- 4zKrqsC
-- Program entry
main = do
    --
    arg_s <- getArgs

    --
    let arg_cnt = length arg_s

    -- 9mlJlKg
    -- If not exactly one command argument is given
    iff (arg_cnt /= 1)
        -- then
        (do
            -- 7rOUXFo
            -- Print program usage
            let usage = strings_join "\n" [
                            "Usage: aoikwinwhich PROG",
                            "",
                            "#/ PROG can be either name or path",
                            "aoikwinwhich notepad.exe",
                            "aoikwinwhich C:\\Windows\\notepad.exe",
                            "",
                            "#/ PROG can be either absolute or relative",
                            "aoikwinwhich C:\\Windows\\notepad.exe",
                            "aoikwinwhich Windows\\notepad.exe",
                            "",
                            "#/ PROG can be either with or without extension",
                            "aoikwinwhich notepad.exe",
                            "aoikwinwhich notepad",
                            "aoikwinwhich C:\\Windows\\notepad.exe",
                            "aoikwinwhich C:\\Windows\\notepad\n"
                            ]

            putStr usage

            -- 3nqHnP7
            exitWith (ExitFailure 1)
        )
        -- else
        (do
            -- 9m5B08H
            -- Get executable name or path
            let prog = head arg_s

            -- 8ulvPXM
            -- Find executable paths
            exe_path_s <- find_exe_paths prog

            -- 5fWrcaF
            -- If has found none
            iff (length exe_path_s == 0)
                -- then
                (do
                    -- 3uswpx0
                    exitWith (ExitFailure 2)
                )
                -- else
                -- If has found some
                (do
                    -- 9xPCWuS
                    -- Print to stdout
                    putStrLn (strings_join "\n" exe_path_s)

                    -- 4s1yY1b
                    exitWith (ExitSuccess)
                )
        )
