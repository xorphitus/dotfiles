local wezterm = require 'wezterm'

return {
  font = wezterm.font_with_fallback {
    'HackGenNerd',
    'Ricty Discord Nerd Font',
    'HackGen',
    'Ricty Discord',
  },
  window_background_opacity = 0.95,
  use_ime = true,
}
