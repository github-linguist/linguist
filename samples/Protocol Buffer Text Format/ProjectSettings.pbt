LeaderboardConfigs {
  Config {
    SortType {
      Value: "mc:ecoresdkleaderboardsorting:higherisbetter"
    }
    FormatType {
      Value: "mc:ecoresdkleaderboardformat:numeric"
    }
    Name: "Top Tea Drinkers"
    Id: "ECC0A05BC3123B7E"
    EntryLimit: 20
  }
}
MainScene: "TeaShopWhiteboxing"
StreamSources {
  Entries {
    StreamIdentifier: "test-stream"
    SourceType {
      Value: "mc:estreamsourcetype:none"
    }
  }
  Entries {
    SourceType {
      Value: "mc:estreamsourcetype:none"
    }
  }
}
