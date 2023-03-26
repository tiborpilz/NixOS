{ pkgs }:
self: super: {

  chatGPT = super.chatGPT.overridePythonAttrs (
    _: {
      src = pkgs.fetchgit {
        url = "https://github.com/mmabrouk/chatgpt-wrapper.git";
        rev = "9b2b207d253ac34385e6878642e743730277643a";
        sha256 = "0c7pzymyj4pd4p5c4ycm9qn184bgadmb84xpzmz4ngrhi29m5wx5";
      };
    }
  );

}
