retry_policy Bar {
  max_retries 3
  strategy {
    type exponential_backoff
  }
}

retry_policy Foo {
  max_retries 3
  strategy {
    type constant_delay
    delay_ms 100
  }
}

client<llm> GPT4 {
  provider openai
  options {
    model gpt-4o
    api_key env.OPENAI_API_KEY
  }
}


client<llm> GPT4o {
  provider openai
  options {
    model gpt-4o
    api_key env.OPENAI_API_KEY
  }
}


client<llm> GPT4Turbo {
  retry_policy Bar
  provider openai
  options {
    model gpt-4-turbo
    api_key env.OPENAI_API_KEY
  }
}

retry_policy GPT4oRetry {
  max_retries 2
  strategy {
    type exponential_backoff
  }
}

client<llm> GPT35 {
  provider openai
  retry_policy GPT4oRetry
  options {
    model "gpt-4o-mini"
    api_key env.OPENAI_API_KEY
  }
}

client<llm> GPT35LegacyProvider {
  provider openai
  options {
    model "gpt-3.5-turbo"
    api_key env.OPENAI_API_KEY
  }
}


client<llm> Ollama {
  provider ollama
  options {
    model llama3.1
  }
}

client<llm> GPT35Azure {
  provider azure-openai
  options {
    resource_name "west-us-azure-baml"
    deployment_id "gpt-35-turbo-default"
    // base_url "https://west-us-azure-baml.openai.azure.com/openai/deployments/gpt-35-turbo-default"
    api_version "2024-02-01"
    api_key env.AZURE_OPENAI_API_KEY
  }
}

// Azure O1 client without max_tokens (should not add default)
client<llm> AzureO1 {
  provider azure-openai
  options {
    resource_name "west-us-azure-baml"
    deployment_id "o1-mini"
    api_version "2024-08-01-preview"
    api_key env.AZURE_OPENAI_API_KEY
    max_tokens null
  }
}

// Azure O1 client with explicit max_tokens (should keep user value)
client<llm> AzureO1WithMaxTokens {
  provider azure-openai
  options {
    resource_name "west-us-azure-baml"
    deployment_id "o1-mini"
    api_version "2024-08-01-preview"
    api_key env.AZURE_OPENAI_API_KEY
    max_tokens 1000
  }
}

client<llm> AzureO1WithMaxCompletionTokens {
  provider azure-openai
  options {
    resource_name "west-us-azure-baml"
    deployment_id "o1-mini"
    api_version "2024-08-01-preview"
    api_key env.AZURE_OPENAI_API_KEY
    max_completion_tokens 1000
  }
}

// Azure GPT-35 client with explicit max_tokens (should keep user value)
client<llm> GPT35AzureWithMaxTokens {
  provider azure-openai
  options {
    resource_name "west-us-azure-baml"
    deployment_id "gpt-35-turbo-default"
    api_version "2024-02-01"
    api_key env.AZURE_OPENAI_API_KEY
    max_tokens 1000
  }
}

// Azure client with invalid resource name (for testing failures)
client<llm> GPT35AzureFailed {
  provider azure-openai
  options {
    resource_name "west-us-azure-baml-incorrect-suffix"
    deployment_id "gpt-35-turbo-default"
    api_key env.AZURE_OPENAI_API_KEY
  }
}

client<llm> Gemini {
  provider google-ai
  options {
    model gemini-1.5-pro-001
    api_key env.GOOGLE_API_KEY
    safetySettings {
      category HARM_CATEGORY_HATE_SPEECH
      threshold BLOCK_LOW_AND_ABOVE
    }
  }
}

client<llm> GeminiOpenAiGeneric {
  provider "openai-generic"
  options {
    base_url "https://generativelanguage.googleapis.com/v1beta/"
    model "gemini-1.5-flash"
    api_key env.GOOGLE_API_KEY
  }
}

client<llm> Vertex {
  provider vertex-ai
  options {
    model gemini-1.5-pro
    location us-central1
    credentials env.INTEG_TESTS_GOOGLE_APPLICATION_CREDENTIALS_CONTENT
  }
}


client<llm> AwsBedrock {
  provider aws-bedrock
  options {
    inference_configuration {
      max_tokens 2048
    }
    // max_tokens 100000
    // max_completion_tokens 100000
    // model "anthropic.claude-3-5-sonnet-20240620-v1:0"
    // model_id "anthropic.claude-3-haiku-20240307-v1:0"
    model "meta.llama3-8b-instruct-v1:0"
    // region "us-east-1"
    // access_key_id env.AWS_ACCESS_KEY_ID
    // secret_access_key env.AWS_SECRET_ACCESS_KEY
    // session_token env.AWS_SESSION_TOKEN
    // session_token null
    // model_id "mistral.mistral-7b-instruct-v0:2"
  }
}

client<llm> AwsBedrockInvalidRegion {
  provider aws-bedrock
  options {
    region "us-invalid-7"
    inference_configuration {
      max_tokens 100
    }
    // model "anthropic.claude-3-5-sonnet-20240620-v1:0"
    // model_id "anthropic.claude-3-haiku-20240307-v1:0"
    model_id "meta.llama3-8b-instruct-v1:0"
    // model_id "mistral.mistral-7b-instruct-v0:2"
  }
}

client<llm> AwsBedrockInvalidAccessKey {
  provider aws-bedrock
  options {
    model_id "meta.llama3-8b-instruct-v1:0"
    access_key_id "AKIAINVALID12345678"
    secret_access_key "abcdef1234567890abcdef1234567890abcdef12"
    inference_configuration {
      max_tokens 100
    }
  }
}

client<llm> AwsBedrockInvalidProfile {
  provider aws-bedrock
  options {
    model_id "meta.llama3-8b-instruct-v1:0"
    profile "boundaryml-dev"
    inference_configuration {
      max_tokens 100
    }
  }
}

client<llm> AwsBedrockInvalidSessionToken {
  provider aws-bedrock
  options {
    model_id "meta.llama3-8b-instruct-v1:0"
    region "us-east-1"
    access_key_id "AKIAINVALID12345678"
    secret_access_key "abcdef1234567890abcdef1234567890abcdef12"
    session_token "invalid-session-token"
    inference_configuration {
      max_tokens 100
    }
  }
}


client<llm> Invalid{
  provider aws-bedrock
  options {
    model_id "meta.llama3-8b-instruct-v1:0"
    region "us-east-1"
    access_key_id "AKIAINVALID12345678"
    secret_access_key "abcdef1234567890abcdef1234567890abcdef12"
    session_token "invalid-session-token"
    inference_configuration {
      max_tokens 100
    }
  }
}

client<llm> Sonnet {
  provider anthropic
  options {
    model claude-3-5-sonnet-20241022
    api_key env.ANTHROPIC_API_KEY
  }
}


client<llm> SonnetThinking {
  provider anthropic
  options {
    model "claude-3-7-sonnet-20250219"
    api_key env.ANTHROPIC_API_KEY
    max_tokens 2048
    thinking {
      type "enabled"
      budget_tokens 1024
    }
  }
}

client<llm> Claude {
  provider anthropic
  options {
    model claude-3-haiku-20240307
    api_key env.ANTHROPIC_API_KEY
    max_tokens 1000
  }
}

client<llm> ClaudeWithCaching {
  provider anthropic
  options {
    model claude-3-haiku-20240307
    api_key env.ANTHROPIC_API_KEY
    max_tokens 500
    allowed_role_metadata ["cache_control"]
    headers {
      "anthropic-beta" "prompt-caching-2024-07-31"
    }
  }
}

client<llm> Resilient_SimpleSyntax {
  retry_policy Foo
  provider baml-fallback
  options {
    strategy [
      GPT4Turbo
      GPT35
      Lottery_SimpleSyntax
    ]
  }
}

client<llm> Lottery_SimpleSyntax {
  provider baml-round-robin
  options {
    start 0
    strategy [
      Claude
      GPT35
    ]
  }
}

client<llm> TogetherAi {
  provider "openai-generic"
  options {
    base_url "https://api.together.ai/v1"
    api_key env.TOGETHER_API_KEY
    model "meta-llama/Llama-3-70b-chat-hf"
  }
}

// OpenAI O1 client without max_tokens (should not add default)
client<llm> OpenAIO1 {
  provider openai
  options {
    model "o1-mini"
    api_key env.OPENAI_API_KEY
  }
}

// OpenAI O1 client with explicit max_tokens (should fail)
client<llm> OpenAIO1WithMaxTokens {
  provider openai
  options {
    model "o1-mini"
    api_key env.OPENAI_API_KEY
    max_tokens 1000
  }
}

// OpenAI O1 client with explicit max_completion_tokens
client<llm> OpenAIO1WithMaxCompletionTokens {
  provider openai
  options {
    model "o1-mini"
    api_key env.OPENAI_API_KEY
    max_completion_tokens 1000
  }
}

// OpenAI GPT-4 client with explicit max_tokens
client<llm> GPT4WithMaxTokens {
  provider openai
  options {
    model "gpt-4"
    api_key env.OPENAI_API_KEY
    max_tokens 1000
  }
}

// Azure O3 client without max_tokens (should not add default)
client<llm> AzureO3 {
  provider azure-openai
  options {
    resource_name "west-us-azure-baml"
    deployment_id "o3-mini"
    api_version "2024-08-01-preview"
    api_key env.AZURE_OPENAI_API_KEY
    max_tokens null
  }
}

// Azure O3 client with explicit max_completion_tokens
client<llm> AzureO3WithMaxCompletionTokens {
  provider azure-openai
  options {
    resource_name "west-us-azure-baml"
    deployment_id "o3-mini"
    api_version "2024-08-01-preview"
    api_key env.AZURE_OPENAI_API_KEY
    max_completion_tokens 1000
  }
}
