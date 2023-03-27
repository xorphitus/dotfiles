local wezterm = require 'wezterm'

return {
  font = wezterm.font_with_fallback {
    'HackGen',
    'Ricty Discord Nerd Font',
    'Ricty Discord',
  },
  window_background_opacity = 0.95,
  use_ime = true,
}
