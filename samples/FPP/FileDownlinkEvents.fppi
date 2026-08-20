@ An error occurred opening a file
event FileOpenError(
                     fileName: string size 100 @< The name of the file
                   ) \
  severity warning high \
  id 0x00 \
  format "Could not open file {}"

@ An error occurred reading a file
event FileReadError(
                     fileName: string size 100 @< The name of the file
                     status: I32 @< The file status of read
                   ) \
  severity warning high \
  id 0x01 \
  format "Could not read file {} with status {}"

@ The File Downlink component successfully sent a file
event FileSent(
                sourceFileName: string size 100 @< The source file name
                destFileName: string size 100 @< The destination file name
              ) \
  severity activity high \
  id 0x02 \
  format "Sent file {} to file {}"

@ The File Downlink component canceled downlink of a file
event DownlinkCanceled(
                        sourceFileName: string size 100 @< The source file name
                        destFileName: string size 100 @< The destination file name
                      ) \
  severity activity high \
  id 0x03 \
  format "Canceled downlink of file {} to file {}"

@ A partial downlink request exceeded the source file size; length was clamped
event DownlinkPartialWarning(
                              startOffset: U32 @< Starting file offset in bytes
                              length: U32 @< Number of bytes to downlink
                              filesize: U32 @< Size of source file
                              sourceFileName: string size 100 @< The source filename
                              destFileName: string size 100 @< The destination file name
                            ) \
  severity warning low \
  id 0x05 \
  format "Offset {} plus length {} is greater than source size {} for partial downlink of file {} to file {}. "

@ A partial downlink request started beyond the source file size and failed
event DownlinkPartialFail(
                           sourceFileName: string size 100 @< The source filename
                           destFileName: string size 100 @< The destination file name
                           startOffset: U32 @< Starting file offset in bytes
                           filesize: U32 @< Size of source file
                         ) \
  severity warning high \
  id 0x06 \
  format "Error occurred during partial downlink of file {} to file {}. Offset {} greater than or equal to source filesize {}."

@ The File Downlink component generated an error when trying to send a data packet.
event SendDataFail(
                    sourceFileName: string size 100 @< The source filename
                    byteOffset: U32 @< Byte offset
                  ) \
  severity warning high \
  id 0x07 \
  format "Failed to send data packet from file {} at byte offset {}."

@ The File Downlink component started a file download.
event SendStarted(
                   fileSize: U32 @< The source file size
                   sourceFileName: string size 100 @< The source filename
                   destFileName: string size 100 @< The destination filename
                 ) \
  severity activity high \
  id 0x08 \
  format "Downlink of {} bytes started from {} to {}"

@ The File Downlink component stopped due to a zero-size file
event DownlinkZeroSizeFile(
                        sourceFileName: string size 100 @< The source file name
                      ) \
  severity warning high \
  id 0x09 \
  format "Downlink of file {} stopped due to zero-size."

@ Supplied filename has overflowed. This intentionally discards the filename to avoid cascading overflows.
event FilenameSourceOverflow() \
  severity warning high \
  id 0x10 \
  format "Commanded source filename too long"

@ Supplied filename has overflowed
event FilenameDestinationOverflow() \
  severity warning high \
  id 0x11 \
  format "Commanded destination filename too long"

@ Source file path is outside the configured read sandbox
event SourceOutOfSandbox(
                        fileName: string size 100 @< The rejected source file path
                      ) \
  severity warning high \
  id 0x12 \
  format "Source file {} is outside the configured read sandbox"