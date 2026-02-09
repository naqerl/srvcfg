" Disable swap files
set noswapfile

" Install plugins
call plug#begin()
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
call plug#end()

" Copy to system clipboard through SSH
function! GetVisualSelection()
    let [lnum1, col1] = getpos("'<")[1:2]
    let [lnum2, col2] = getpos("'>")[1:2]
    let lines = getline(lnum1, lnum2)
    if lnum1 == 0 && lnum2 == 0 && col1 == 0 && col2 == 0
        return ''
    endif
    let lines[-1] = lines[-1][:col2 - (&selection == 'inclusive' ? 1 : 2)]
    let lines[0] = lines[0][col1 - 1:]
    return join(lines, "\n")
endfunction

function! OscCopyRange()
  let text = GetVisualSelection()
  if empty(text)
    echo "No text selected"
    return
  endif
  let encodedText=text
  let encodedText=substitute(text, '\', '\\\\', "g")
  let encodedText=substitute(encodedText, "'", "'\\\\''", "g")
  let executeCmd="echo -n '".encodedText."' | base64 | tr -d '\\n'"
  let encodedText=system(executeCmd)
  " Remove trailing newline from base64 output
  let encodedText=substitute(encodedText, '\n$', '', '')
  if $TMUX != ""
    " For tmux: wrap OSC 52 in DCS passthrough and write directly to /dev/tty
    call writefile(["\ePtmux;\e\e]52;c;" . encodedText . "\e\e\\\e\\"], "/dev/tty", "b")
  else
    " For regular terminals: direct OSC 52 to /dev/tty
    call writefile(["\e]52;c;" . encodedText . "\x07"], "/dev/tty", "b")
  endif
  redraw!
  echo "Copied to clipboard"
endfunction

vnoremap <leader>y :<C-u>call OscCopyRange()<CR>
