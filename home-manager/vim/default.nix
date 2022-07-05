{ pkgs, ... }:
let
  ## Define Plugins as list Nix packages

  ## UI Elements
  plugins = with pkgs.vimPlugins; ([
    ale
    The_NERD_tree
    vim-gitgutter
    vim-airline
    vim-airline-themes
    cpsm
    tagbar
    xterm-color-table-vim
    goyo-vim
    ## Color & Eyecandy
    base16-vim
    awesome-vim-colorschemes
    changeColorScheme-vim
    vim-devicons
    ## Language Features / Editing help
    vim-autoformat
    tcomment_vim
    vim-markdown
    colorizer
    vim-surround
    tmux-complete-vim
    unicode-vim
    vim-abolish
    vim-asterisk
    vim-easymotion
    agda-vim
    align
    julia-vim
    ### CoC is a language server implementation with extensions for all kinds of languages
    coc-nvim
    coc-css
    coc-eslint
    coc-go
    coc-html
    coc-java
    coc-jest
    coc-json
    coc-lists
    coc-prettier
    coc-python
    coc-r-lsp
    coc-rls
    coc-stylelint
    coc-tabnine
    coc-tslint-plugin
    coc-vimtex
    coc-yaml
    coc-yank
    ## Tooling
    vim-dispatch
    auto-git-diff
    vimwiki
    wmgraphviz-vim
    ultisnips
    fugitive
  ]);
in
{
  # Install Vim itself
  programs.vim = {
    enable = true;
    plugins = plugins;
    settings = {
      expandtab = true;
      tabstop = 2;
      relativenumber = true;
    };
    extraConfig = ''
      let mapleader = ","
      map <leader>n :NERDTreeToggle<cr>

      let g:ale_sign_column_always=1
      let g:airline#extensions#ale#enabled = 1
      let base16colorspace=256

      au BufNewFile,BufRead *.agda setf agda
    '';
  };
}
