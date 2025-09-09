class ReceiptItem {
  name string
  description string?
  quantity int
  price float
}

class ReceiptInfo {
    items ReceiptItem[]
    total_cost float?
    venue "barisa" | "ox_burger"
}

function ExtractReceiptInfo(email: string, reason: "curiosity" | "personal_finance") -> ReceiptInfo {
  client GPT4o
  prompt #"
    Given the receipt below:

    ```
    {{email}}
    ```

    {{ ctx.output_format }}
  "#
}

