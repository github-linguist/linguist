let Text/concatMap = ../../../Prelude/Text/concatMap

let Text/concatSep = ../../../Prelude/Text/concatSep

let Row =
      { cores :
          Natural
      , host :
          Text
      , key :
          Text
      , mandatoryFeatures :
          List Text
      , platforms :
          List Text
      , speedFactor :
          Natural
      , supportedFeatures :
          List Text
      , user :
          Optional Text
      }

let renderRow =
        λ ( row
          : Row
          )
      → let host =
              Optional/fold
              Text
              row.user
              Text
              (λ(user : Text) → "${user}@${row.host}")
              row.host
        
        let platforms = Text/concatSep "," row.platforms
        
        let key = row.key
        
        let cores = Integer/show (Natural/toInteger row.cores)
        
        let speedFactor = Integer/show (Natural/toInteger row.speedFactor)
        
        let supportedFeatures = Text/concatSep "," row.supportedFeatures
        
        let mandatoryFeatures = Text/concatSep "," row.mandatoryFeatures
        
        in  ''
            ${host} ${platforms} ${key} ${cores} ${speedFactor} ${supportedFeatures} ${mandatoryFeatures}
            ''

in  Text/concatMap Row renderRow
