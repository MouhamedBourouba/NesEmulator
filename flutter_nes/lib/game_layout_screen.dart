import 'dart:typed_data';
import 'package:flutter/material.dart';
import 'package:flutter_nes/emulator_screen.dart';
import 'package:file_selector/file_selector.dart';
import 'package:hive_ce/hive.dart';
import 'package:flutter_nes/settings_screen.dart';

class GamesLayoutScreen extends StatefulWidget {
  const GamesLayoutScreen({super.key});

  @override
  State<GamesLayoutScreen> createState() => _GamesLayoutScreenState();
}

class _GamesLayoutScreenState extends State<GamesLayoutScreen> {
  late Box _gamesBox;
  bool _isGridView = false;

  @override
  void initState() {
    super.initState();
    _gamesBox = Hive.box('games');
  }

  Future<void> _pickRom() async {
    const typeGroup = XTypeGroup(label: 'NES ROMs');
    final file = await openFile(acceptedTypeGroups: [typeGroup]);
    if (file == null) return;

    final romBytes = await file.readAsBytes();
    final fileName = file.name;

    // Save to Hive
    final gameData = {
      'name': fileName,
      'romData': romBytes,
      'addedAt': DateTime.now().toIso8601String(),
    };

    // Use filename as key to prevent duplicates
    await _gamesBox.put(fileName, gameData);
    
    // Refresh UI
    setState(() {});
  }

  void _deleteGame(String key) async {
    showDialog(
      context: context,
      builder: (context) => AlertDialog(
        title: const Text('Delete Game'),
        content: const Text('Are you sure you want to remove this game?'),
        actions: [
          TextButton(
            onPressed: () => Navigator.pop(context),
            child: const Text('Cancel'),
          ),
          TextButton(
            onPressed: () async {
              await _gamesBox.delete(key);
              Navigator.pop(context);
              setState(() {});
            },
            child: const Text('Delete', style: TextStyle(color: Colors.redAccent)),
          ),
        ],
      ),
    );
  }

  void _launchGame(Uint8List romData) {
    Navigator.push(
      context,
      MaterialPageRoute(
        builder: (context) => EmulatorScreen(romData: romData),
      ),
    );
  }

  @override
  Widget build(BuildContext context) {
    // Read items from Hive
    final games = _gamesBox.keys.map((key) {
      final value = _gamesBox.get(key) as Map;
      return {
        'key': key.toString(),
        'name': value['name'] as String,
        'romData': value['romData'] as Uint8List,
      };
    }).toList();

    return Scaffold(
      appBar: AppBar(
        title: const Text(
          "My Library",
          style: TextStyle(fontWeight: FontWeight.bold, fontSize: 24),
        ),
        actions: [
          IconButton(
            icon: Icon(_isGridView ? Icons.view_list_rounded : Icons.grid_view_rounded),
            tooltip: _isGridView ? 'Switch to List View' : 'Switch to Grid View',
            onPressed: () {
              setState(() {
                _isGridView = !_isGridView;
              });
            },
          ),
          IconButton(
            icon: const Icon(Icons.settings_rounded),
            tooltip: 'Settings',
            onPressed: () {
              Navigator.push(context, MaterialPageRoute(builder: (_) => const SettingsScreen()));
            },
          ),
          const SizedBox(width: 8),
        ],
      ),
      body: games.isEmpty
          ? Center(
              child: Column(
                mainAxisAlignment: MainAxisAlignment.center,
                children: [
                  Icon(Icons.gamepad_rounded, size: 80, color: Colors.grey.withOpacity(0.3)),
                  const SizedBox(height: 16),
                  const Text(
                    "No games imported yet.",
                    style: TextStyle(fontSize: 18, color: Colors.grey),
                  ),
                  const SizedBox(height: 8),
                  const Text(
                    "Tap the button below to add a NES ROM.",
                    style: TextStyle(fontSize: 14, color: Colors.grey),
                  ),
                ],
              ),
            )
          : (_isGridView ? _buildGridView(games) : _buildListView(games)),
      floatingActionButton: FloatingActionButton.extended(
        onPressed: _pickRom,
        tooltip: "Import Game",
        icon: const Icon(Icons.add),
        label: const Text("Import ROM", style: TextStyle(fontWeight: FontWeight.bold)),
        backgroundColor: Theme.of(context).colorScheme.primary,
        foregroundColor: Colors.white,
      ),
    );
  }

  Widget _buildGridView(List<Map<String, dynamic>> games) {
    return GridView.builder(
      padding: const EdgeInsets.all(16),
      gridDelegate: const SliverGridDelegateWithMaxCrossAxisExtent(
        maxCrossAxisExtent: 220,
        crossAxisSpacing: 16,
        mainAxisSpacing: 16,
        childAspectRatio: 0.85,
      ),
      itemCount: games.length,
      itemBuilder: (context, index) {
        final game = games[index];
        return _GameCard(
          name: game['name'],
          onTap: () => _launchGame(game['romData']),
          onLongPress: () => _deleteGame(game['key']),
        );
      },
    );
  }

  Widget _buildListView(List<Map<String, dynamic>> games) {
    return ListView.builder(
      padding: const EdgeInsets.symmetric(horizontal: 16, vertical: 12),
      itemCount: games.length,
      itemBuilder: (context, index) {
        final game = games[index];
        return Padding(
          padding: const EdgeInsets.only(bottom: 12),
          child: _GameListTile(
            name: game['name'],
            onTap: () => _launchGame(game['romData']),
            onLongPress: () => _deleteGame(game['key']),
          ),
        );
      },
    );
  }
}

class _GameCard extends StatelessWidget {
  final String name;
  final VoidCallback onTap;
  final VoidCallback onLongPress;

  const _GameCard({
    required this.name,
    required this.onTap,
    required this.onLongPress,
  });

  @override
  Widget build(BuildContext context) {
    final title = name.replaceAll(RegExp(r'\.nes$', caseSensitive: false), '');
    
    return Material(
      color: Colors.transparent,
      child: InkWell(
        onTap: onTap,
        onLongPress: onLongPress,
        borderRadius: BorderRadius.circular(16),
        child: Ink(
          decoration: BoxDecoration(
            borderRadius: BorderRadius.circular(16),
            color: Theme.of(context).colorScheme.surface,
            border: Border.all(color: Colors.white.withOpacity(0.05), width: 1),
            boxShadow: [
              BoxShadow(
                color: Theme.of(context).brightness == Brightness.dark
                    ? Colors.black.withOpacity(0.4)
                    : Colors.black.withOpacity(0.08),
                blurRadius: 10,
                offset: const Offset(0, 4),
              ),
            ],
          ),
          child: Column(
            crossAxisAlignment: CrossAxisAlignment.stretch,
            children: [
              Expanded(
                child: Container(
                  decoration: BoxDecoration(
                    borderRadius: const BorderRadius.vertical(top: Radius.circular(16)),
                    gradient: LinearGradient(
                      begin: Alignment.topLeft,
                      end: Alignment.bottomRight,
                      colors: [
                        Theme.of(context).colorScheme.primary.withOpacity(0.85),
                        Theme.of(context).colorScheme.secondary.withOpacity(0.85),
                      ],
                    ),
                  ),
                  child: const Center(
                    child: Icon(
                      Icons.videogame_asset_rounded,
                      size: 64,
                      color: Colors.white70,
                    ),
                  ),
                ),
              ),
              Container(
                padding: const EdgeInsets.all(12),
                child: Text(
                  title,
                  style: const TextStyle(
                    fontWeight: FontWeight.w600,
                    fontSize: 16,
                  ),
                  maxLines: 2,
                  overflow: TextOverflow.ellipsis,
                ),
              ),
            ],
          ),
        ),
      ),
    );
  }
}

class _GameListTile extends StatelessWidget {
  final String name;
  final VoidCallback onTap;
  final VoidCallback onLongPress;

  const _GameListTile({
    required this.name,
    required this.onTap,
    required this.onLongPress,
  });

  @override
  Widget build(BuildContext context) {
    final title = name.replaceAll(RegExp(r'\.nes$', caseSensitive: false), '');
    
    return Material(
      color: Colors.transparent,
      child: InkWell(
        onTap: onTap,
        onLongPress: onLongPress,
        borderRadius: BorderRadius.circular(12),
        child: Ink(
          padding: const EdgeInsets.all(12),
          decoration: BoxDecoration(
            borderRadius: BorderRadius.circular(12),
            color: Theme.of(context).colorScheme.surface,
            border: Border.all(color: Colors.white.withOpacity(0.05), width: 1),
            boxShadow: [
              BoxShadow(
                color: Theme.of(context).brightness == Brightness.dark
                    ? Colors.black.withOpacity(0.3)
                    : Colors.black.withOpacity(0.07),
                blurRadius: 6,
                offset: const Offset(0, 3),
              ),
            ],
          ),
          child: Row(
            children: [
              Container(
                width: 60,
                height: 60,
                decoration: BoxDecoration(
                  borderRadius: BorderRadius.circular(10),
                  gradient: LinearGradient(
                    begin: Alignment.topLeft,
                    end: Alignment.bottomRight,
                    colors: [
                      Theme.of(context).colorScheme.primary.withOpacity(0.85),
                      Theme.of(context).colorScheme.secondary.withOpacity(0.85),
                    ],
                  ),
                ),
                child: const Icon(
                  Icons.videogame_asset_rounded,
                  color: Colors.white70,
                ),
              ),
              const SizedBox(width: 16),
              Expanded(
                child: Text(
                  title,
                  style: const TextStyle(
                    fontWeight: FontWeight.w600,
                    fontSize: 18,
                  ),
                  maxLines: 2,
                  overflow: TextOverflow.ellipsis,
                ),
              ),
              const Icon(Icons.play_circle_fill_rounded, color: Colors.white54, size: 36),
            ],
          ),
        ),
      ),
    );
  }
}
