setFileSize :: FilePath -> FileOffset -> IO ()
-- Truncates the file down to the specified length.
-- If the file was larger than the given length
-- before this operation was performed the extra is lost.
-- Note: calls truncate.
