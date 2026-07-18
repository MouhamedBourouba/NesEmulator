import 'package:flutter/foundation.dart';
import 'package:hive_ce/hive.dart';

class AppSettings {
  static final AppSettings _instance = AppSettings._internal();
  factory AppSettings() => _instance;
  AppSettings._internal();

  late Box _box;
  final ValueNotifier<bool> isDarkMode = ValueNotifier(true);
  final ValueNotifier<bool> showFps = ValueNotifier(false);

  Future<void> init() async {
    _box = await Hive.openBox('settings');
    isDarkMode.value = _box.get('isDarkMode', defaultValue: true);
    showFps.value = _box.get('showFps', defaultValue: false);

    isDarkMode.addListener(() {
      _box.put('isDarkMode', isDarkMode.value);
    });
    showFps.addListener(() {
      _box.put('showFps', showFps.value);
    });
  }
}
