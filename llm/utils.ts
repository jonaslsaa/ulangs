
export type OpenAIEnv = {
    apiKey: string;
    model: string;
    baseUrl?: string;
};

export function loadOpenAIEnvVars(): OpenAIEnv {
  function getEnvVar(name: string): string | undefined {
      const value = process.env[name];
      if (value === undefined || value === '') {
          return undefined;
      }
      return value;
  }

  const openaiBaseUrl = getEnvVar('OPENAI_COMPATIBLE_BASEURL');
  const openaiApiKey = getEnvVar('OPENAI_COMPATIBLE_API_KEY');
  const openaiModel = getEnvVar('OPENAI_COMPATIBLE_MODEL');
  if (openaiApiKey === undefined || openaiModel === undefined) {
      throw new Error('OpenAI environment variables not set');
  }

  return {
      baseUrl: openaiBaseUrl,
      apiKey: openaiApiKey,
      model: openaiModel,
  };
}