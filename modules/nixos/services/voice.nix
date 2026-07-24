{ config, lib, pkgs, ... }:

with lib;
with lib.my;

let
  cfg = config.modules.services.voice;
in
{
  # Fully-local Home Assistant voice pipeline compute. Home Assistant itself runs
  # elsewhere (192.168.1.175) and reaches these over the LAN via the Wyoming
  # protocol / Ollama HTTP API. The GTX 1080 accelerates the LLM; Whisper stays
  # on CPU so the 8 GB of VRAM is spent entirely on the conversation agent.
  options.modules.services.voice = {
    enable = mkBoolOpt false;

    whisperModel = mkOpt' types.str "Systran/faster-distil-whisper-small.en" ''
      faster-whisper model for speech-to-text. Runs on CPU; the distil-small.en
      model is a large accuracy upgrade over the tiny-int8 default while staying
      fast enough for short voice commands.
    '';

    piperVoice = mkOpt' types.str "en_US-lessac-medium" ''
      Piper text-to-speech voice. Voices are downloaded automatically on first use.
    '';

    llmModel = mkOpt' types.str "qwen2.5:7b" ''
      Ollama model used as the conversation agent. A q4 7-8B model (~5 GB) fits the
      1080's 8 GB VRAM. Alternatives: "llama3.1:8b", or "llama3.2:3b" for lower latency.
    '';

    wakeword = mkBoolOpt true;
  };

  config = mkIf cfg.enable {
    # Speech-to-text (Wyoming). CPU by design — see module comment above.
    services.wyoming.faster-whisper.servers.ha = {
      enable = true;
      model = cfg.whisperModel;
      language = "en";
      device = "cpu";
      uri = "tcp://0.0.0.0:10300";
    };

    # Text-to-speech (Wyoming).
    services.wyoming.piper.servers.ha = {
      enable = true;
      voice = cfg.piperVoice;
      uri = "tcp://0.0.0.0:10200";
    };

    # Central wake-word detection (Wyoming). Useful for mic-only satellites; the
    # "ok_nabu" model matches Home Assistant's / Voice PE default wake word.
    services.wyoming.openwakeword = mkIf cfg.wakeword {
      enable = true;
      uri = "tcp://0.0.0.0:10400";
      preloadModels = [ "ok_nabu" ];
    };

    # Local LLM conversation agent, GPU-accelerated on the 1080.
    services.ollama = {
      enable = true;
      acceleration = "cuda";
      # Bind to all interfaces so Home Assistant (192.168.1.175) can reach it;
      # klaus runs with the firewall disabled so no port needs opening.
      host = "0.0.0.0";
      loadModels = [ cfg.llmModel ];
      # Keep the model resident — this box is a dedicated assistant, and reloading
      # a multi-GB model on every request adds seconds of latency.
      environmentVariables.OLLAMA_KEEP_ALIVE = "-1";
    };
  };
}
