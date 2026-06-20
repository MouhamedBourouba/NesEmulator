import 'package:flutter/material.dart';

void main() {
  loadNesLib();

  runApp(const MainApp());
}

void loadNesLib() {}

class MainApp extends StatelessWidget {
  const MainApp({super.key});

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      debugShowCheckedModeBanner: false,
      home: Scaffold(
        appBar: AppBar(
          title: Text("Nes emulator"),
          backgroundColor: Colors.amber,
        ),
        body: GamesLayout(),
        floatingActionButton: FloatingActionButton(
          onPressed: () {
            print("pressed game add button");
          },
          tooltip: "Add Game",
          child: Icon(Icons.upload),
        ),
      ),
    );
  }
}

class GamesLayout extends StatelessWidget {
  const GamesLayout({super.key});

  @override
  Widget build(BuildContext context) {
    return Center(child: Text("Games here"));
  }
}
