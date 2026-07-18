import 'package:flutter/material.dart';
import 'package:flutter_nes/settings.dart';

class SettingsScreen extends StatelessWidget {
  const SettingsScreen({super.key});

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text("Settings", style: TextStyle(fontWeight: FontWeight.bold)),
      ),
      body: ListView(
        padding: const EdgeInsets.all(16),
        children: [
          const Text(
            "Appearance",
            style: TextStyle(fontWeight: FontWeight.bold, fontSize: 16, color: Colors.grey),
          ),
          const SizedBox(height: 8),
          ValueListenableBuilder<bool>(
            valueListenable: AppSettings().isDarkMode,
            builder: (context, isDark, _) {
              return SwitchListTile(
                title: const Text("Dark Theme", style: TextStyle(fontWeight: FontWeight.w600)),
                subtitle: const Text("Toggle between light and dark mode"),
                value: isDark,
                onChanged: (val) => AppSettings().isDarkMode.value = val,
                secondary: const Icon(Icons.brightness_4_rounded),
                shape: RoundedRectangleBorder(borderRadius: BorderRadius.circular(12)),
                tileColor: Theme.of(context).colorScheme.surface,
              );
            },
          ),
          const SizedBox(height: 24),
          const Text(
            "Emulation",
            style: TextStyle(fontWeight: FontWeight.bold, fontSize: 16, color: Colors.grey),
          ),
          const SizedBox(height: 8),
          ValueListenableBuilder<bool>(
            valueListenable: AppSettings().showFps,
            builder: (context, showFps, _) {
              return SwitchListTile(
                title: const Text("Show FPS Counter", style: TextStyle(fontWeight: FontWeight.w600)),
                subtitle: const Text("Display real-time rendering performance"),
                value: showFps,
                onChanged: (val) => AppSettings().showFps.value = val,
                secondary: const Icon(Icons.speed_rounded),
                shape: RoundedRectangleBorder(borderRadius: BorderRadius.circular(12)),
                tileColor: Theme.of(context).colorScheme.surface,
              );
            },
          ),
        ],
      ),
    );
  }
}
