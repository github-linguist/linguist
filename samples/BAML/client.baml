client<llm> LOCAL_FINETUNED { 
  provider "openai-generic"
  options {
    base_url "http://localhost:11434/v1"
    model "rbrgs-finetuning"
    max_tokens 4096
  }
}

client<llm> GEMINI_PRO_2_5 {
  provider "openai-generic"
  retry_policy Constant
  options {
    base_url https://openrouter.ai/api/v1
    api_key env.OPENROUTER_API_KEY
    model "google/gemini-2.5-pro"
    max_tokens 4096
  }
}

client<llm> GEMINI_FLASH_2_5 {
  provider "openai-generic"
  retry_policy Constant
  options {
    base_url https://openrouter.ai/api/v1
    api_key env.OPENROUTER_API_KEY
    model "google/gemini-2.5-flash"
    max_tokens 4096
  }
}

client<llm> OPENAI_O4_MINI {
  provider "openai-generic"
  retry_policy Constant
  options {
    base_url https://openrouter.ai/api/v1
    api_key env.OPENROUTER_API_KEY
    model "openai/o4-mini"
    max_tokens 4096
  }
}

client<llm> OPENAI_GPT_4_1_MINI {
  provider "openai-generic"
  retry_policy Constant
  options {
    base_url https://openrouter.ai/api/v1
    api_key env.OPENROUTER_API_KEY
    model "openai/gpt-4.1-mini"
  }
}

client<llm> ANTHROPIC_CLAUDE_SONNET_4 {
  provider "openai-generic"
  retry_policy Constant
  options {
    base_url https://openrouter.ai/api/v1
    api_key env.OPENROUTER_API_KEY
    model "anthropic/claude-sonnet-4"
  }
}

client<llm> META_LLAMA_3_3_8B_IT_FREE {
  provider "openai-generic"
  retry_policy Constant
  options {
    base_url https://openrouter.ai/api/v1
    api_key env.OPENROUTER_API_KEY
    model "meta-llama/llama-3.3-8b-instruct:free"
    max_tokens 4096
  }
}

client<llm> META_LLAMA_3_3_70B {
  provider "openai-generic"
  retry_policy Constant
  options {
    base_url https://openrouter.ai/api/v1
    api_key env.OPENROUTER_API_KEY
    model "meta-llama/llama-3.3-70b-instruct"
  }
}

// This isnt working
client<llm> META_LLAMA_2_70B_HF {
  provider "openai-generic"
  retry_policy Constant
  options {
    base_url https://router.huggingface.co/together/v1
    api_key env.HF_TOKEN
    model "meta-llama/Llama-2-70b-hf"
    max_tokens 2048
  }
}

client<llm> LOCAL_R1_32B {
  provider "openai-generic"
  options {
    base_url "http://localhost:11434/v1"
    model "deepseek-r1:32b"
    max_tokens 4096
  }
}

client<llm> LOCAL_QWEN3_0_6B { 
  provider "openai-generic"
  options {
    base_url "http://localhost:11434/v1"
    model "qwen3:0.6b"
    max_tokens 4096
  }
}


retry_policy Constant {
  max_retries 4
  // Strategy is optional
  strategy {
    type constant_delay
    delay_ms 200
  }
}