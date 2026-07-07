import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'package:flutter/foundation.dart';
import 'dart:async';
import 'dart:isolate';
import 'dart:ui' as ui;
import 'package:flutter_nes/libnes.dart';
import 'package:flutter_nes/settings.dart';

final Map<String, bool> nesButtonState = {
  'UP': false,
  'DOWN': false,
  'LEFT': false,
  'RIGHT': false,
  'A': false,
  'B': false,
  'START': false,
  'SELECT': false,
};

void setNesButton(String button, bool pressed) {
  nesButtonState[button] = pressed;
}

void nesEmulatorEntry(SendPort port) {
  final commandPort = ReceivePort();
  port.send(commandPort.sendPort);

  commandPort.listen((message) {
    if (message is Map) {
      final button = message['button'];
      final pressed = message['pressed'];
      if (button is String && pressed is bool) {
        nesButtonState[button] = pressed;
      }
    }
  });

  const frameDuration = Duration(milliseconds: 16);

  Timer.periodic(frameDuration, (_) {
    if (LibNes().nesIsInitialized()) {
      LibNes().nesSetInputControllerA(
        a: nesButtonState["A"] ?? false,
        b: nesButtonState["B"] ?? false,
        select: nesButtonState["SELECT"] ?? false,
        start: nesButtonState["START"] ?? false,
        up: nesButtonState["UP"] ?? false,
        down: nesButtonState["DOWN"] ?? false,
        left: nesButtonState["LEFT"] ?? false,
        right: nesButtonState["RIGHT"] ?? false,
      );

      LibNes().nesFrame();
      port.send(LibNes().nesFrameBuffer());
    }
  });
}

class EmulatorScreen extends StatefulWidget {
  final Uint8List romData;
  const EmulatorScreen({super.key, required this.romData});

  @override
  State<EmulatorScreen> createState() => _EmulatorScreenState();
}

class _EmulatorScreenState extends State<EmulatorScreen> {
  Isolate? _emulatorIsolate;
  ReceivePort? _emulatorPort;
  SendPort? _commandPort;
  ui.Image? _currentFrameImage;
  Timer? _inputPollTimer;
  late bool _showControls;

  int _frameCount = 0;
  DateTime _lastFpsTime = DateTime.now();
  int _currentFps = 0;

  // Last state we actually sent to the isolate, so we only send diffs.
  final Map<String, bool> _lastSent = Map.of(nesButtonState);

  bool _handleKeyEvent(KeyEvent event) {
    if (event is KeyDownEvent) {
      _setKeyFromLogical(event.logicalKey, true);
    } else if (event is KeyUpEvent) {
      _setKeyFromLogical(event.logicalKey, false);
    }
    return false; // Let other handlers process it too if needed
  }

  void _setKeyFromLogical(LogicalKeyboardKey key, bool pressed) {
    if (key == LogicalKeyboardKey.arrowUp || key == LogicalKeyboardKey.keyW) setNesButton('UP', pressed);
    if (key == LogicalKeyboardKey.arrowDown || key == LogicalKeyboardKey.keyS) setNesButton('DOWN', pressed);
    if (key == LogicalKeyboardKey.arrowLeft || key == LogicalKeyboardKey.keyA) setNesButton('LEFT', pressed);
    if (key == LogicalKeyboardKey.arrowRight || key == LogicalKeyboardKey.keyD) setNesButton('RIGHT', pressed);
    
    // Z/X or J/K for A/B
    if (key == LogicalKeyboardKey.keyX || key == LogicalKeyboardKey.keyK) setNesButton('A', pressed);
    if (key == LogicalKeyboardKey.keyZ || key == LogicalKeyboardKey.keyJ) setNesButton('B', pressed);
    
    if (key == LogicalKeyboardKey.enter) setNesButton('START', pressed);
    if (key == LogicalKeyboardKey.shiftLeft || key == LogicalKeyboardKey.shiftRight) setNesButton('SELECT', pressed);
  }

  Future<void> _onNewFrame(Uint8List rgbaBytes) async {
    final buffer = await ui.ImmutableBuffer.fromUint8List(rgbaBytes);
    final descriptor = ui.ImageDescriptor.raw(
      buffer,
      width: 256,
      height: 240,
      pixelFormat: ui.PixelFormat.rgba8888,
    );
    final codec = await descriptor.instantiateCodec();
    final frame = await codec.getNextFrame();
    final oldImage = _currentFrameImage;
    
    _frameCount++;
    final now = DateTime.now();
    if (now.difference(_lastFpsTime).inSeconds >= 1) {
      _currentFps = _frameCount;
      _frameCount = 0;
      _lastFpsTime = now;
    }

    if (mounted) {
      setState(() => _currentFrameImage = frame.image);
    }
    oldImage?.dispose();
  }

  void _setupEmulatorIsolate() async {
    _emulatorPort = ReceivePort();

    _emulatorIsolate = await Isolate.spawn(
      nesEmulatorEntry,
      _emulatorPort!.sendPort,
    );

    _emulatorPort!.listen((message) {
      if (message is SendPort) {
        _commandPort = message; // isolate handed us its inbox for input
      } else if (message is Uint8List) {
        _onNewFrame(message);
      }
    });
  }

  void _flushInputToIsolate() {
    final port = _commandPort;
    if (port == null) return;
    nesButtonState.forEach((button, pressed) {
      if (_lastSent[button] != pressed) {
        port.send({'button': button, 'pressed': pressed});
        _lastSent[button] = pressed;
      }
    });
  }

  void _startInputPolling() {
    _inputPollTimer = Timer.periodic(
      const Duration(milliseconds: 16),
      (_) => _flushInputToIsolate(),
    );
  }

  @override
  void initState() {
    super.initState();
    _showControls = defaultTargetPlatform == TargetPlatform.android || defaultTargetPlatform == TargetPlatform.iOS;
    HardwareKeyboard.instance.addHandler(_handleKeyEvent);
    
    // Allow landscape and portrait
    SystemChrome.setPreferredOrientations([
      DeviceOrientation.portraitUp,
      DeviceOrientation.portraitDown,
      DeviceOrientation.landscapeLeft,
      DeviceOrientation.landscapeRight,
    ]);

    if (LibNes().nesInit(widget.romData)) {
      _setupEmulatorIsolate();
      _startInputPolling();
    } else {
      Navigator.pop(context);
    }
  }

  @override
  void dispose() {
    HardwareKeyboard.instance.removeHandler(_handleKeyEvent);
    _inputPollTimer?.cancel();
    _emulatorPort?.close();
    _emulatorIsolate?.kill(priority: Isolate.immediate);
    _currentFrameImage?.dispose();
    nesButtonState.updateAll((key, value) => false);
    super.dispose();
  }

  Widget _buildGameScreen() {
    return Container(
      decoration: BoxDecoration(
        color: Colors.black,
        boxShadow: [
          BoxShadow(
            color: Theme.of(context).colorScheme.primary.withOpacity(0.15),
            blurRadius: 40,
            spreadRadius: 5,
          ),
        ],
        border: Border.all(color: Colors.black, width: 4),
        borderRadius: BorderRadius.circular(12),
      ),
      clipBehavior: Clip.antiAlias,
      child: _currentFrameImage == null
          ? const SizedBox(
              width: 256,
              height: 240,
              child: Center(child: CircularProgressIndicator()),
            )
          : AspectRatio(
              aspectRatio: 256 / 240,
              child: Stack(
                fit: StackFit.expand,
                children: [
                  CustomPaint(
                    painter: _FramePainter(_currentFrameImage!),
                  ),
                  ValueListenableBuilder<bool>(
                    valueListenable: AppSettings().showFps,
                    builder: (context, showFps, child) {
                      if (!showFps) return const SizedBox.shrink();
                      return Positioned(
                        top: 8,
                        right: 8,
                        child: Container(
                          padding: const EdgeInsets.symmetric(horizontal: 6, vertical: 2),
                          decoration: BoxDecoration(
                            color: Colors.black.withOpacity(0.6),
                            borderRadius: BorderRadius.circular(4),
                          ),
                          child: Text(
                            '$_currentFps FPS',
                            style: const TextStyle(
                              color: Colors.greenAccent,
                              fontWeight: FontWeight.bold,
                              fontSize: 12,
                            ),
                          ),
                        ),
                      );
                    },
                  ),
                ],
              ),
            ),
    );
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      backgroundColor: const Color(0xFF0F1014),
      appBar: AppBar(
        backgroundColor: Colors.transparent,
        elevation: 0,
        leading: IconButton(
          icon: const Icon(Icons.arrow_back_rounded, color: Colors.white),
          onPressed: () => Navigator.pop(context),
          tooltip: 'Exit Game',
        ),
        actions: [
          IconButton(
            icon: Icon(
              _showControls ? Icons.videogame_asset_off_rounded : Icons.videogame_asset_rounded,
              color: Colors.white,
            ),
            onPressed: () {
              setState(() {
                _showControls = !_showControls;
              });
            },
            tooltip: 'Toggle Controls',
          ),
          const SizedBox(width: 8),
        ],
      ),
      body: SafeArea(
        child: LayoutBuilder(
          builder: (context, constraints) {
            final isLandscape = constraints.maxWidth > constraints.maxHeight;
            if (isLandscape) {
              return Row(
                children: [
                  if (_showControls)
                    const Expanded(
                      child: Center(child: _DPad()),
                    ),
                  Expanded(
                    flex: 3,
                    child: Center(
                      child: Padding(
                        padding: const EdgeInsets.symmetric(vertical: 24.0),
                        child: _buildGameScreen(),
                      ),
                    ),
                  ),
                  if (_showControls)
                    Expanded(
                      child: Column(
                      mainAxisAlignment: MainAxisAlignment.center,
                      children: [
                        Row(
                          mainAxisAlignment: MainAxisAlignment.center,
                          children: const [
                            _PillButton(label: 'SELECT'),
                            SizedBox(width: 16),
                            _PillButton(label: 'START'),
                          ],
                        ),
                        const SizedBox(height: 48),
                        Row(
                          mainAxisAlignment: MainAxisAlignment.center,
                          crossAxisAlignment: CrossAxisAlignment.end,
                          children: const [
                            _RoundButton(label: 'B', color: Color(0xFFFF6584)),
                            SizedBox(width: 24),
                            Padding(
                              padding: EdgeInsets.only(bottom: 24),
                              child: _RoundButton(label: 'A', color: Color(0xFFFF6584), size: 64),
                            ),
                          ],
                        ),
                      ],
                    ),
                  ),
                ],
              );
            } else {
              return Column(
                children: [
                  Expanded(
                    child: Center(
                      child: Padding(
                        padding: const EdgeInsets.all(16.0),
                        child: _buildGameScreen(),
                      ),
                    ),
                  ),
                  if (_showControls)
                    Container(
                      padding: const EdgeInsets.fromLTRB(28, 20, 28, 28),
                    decoration: BoxDecoration(
                      gradient: const LinearGradient(
                        begin: Alignment.topCenter,
                        end: Alignment.bottomCenter,
                        colors: [Color(0xFF1A1B23), Color(0xFF0F1014)],
                      ),
                      border: const Border(
                        top: BorderSide(color: Color(0xFF2A2B36), width: 1),
                      ),
                      boxShadow: [
                        BoxShadow(
                          color: Colors.black.withOpacity(0.5),
                          blurRadius: 10,
                          offset: const Offset(0, -5),
                        )
                      ],
                    ),
                    child: Row(
                      crossAxisAlignment: CrossAxisAlignment.center,
                      mainAxisAlignment: MainAxisAlignment.spaceBetween,
                      children: [
                        const _DPad(),
                        Column(
                          mainAxisSize: MainAxisSize.min,
                          children: [
                            Row(
                              children: const [
                                _PillButton(label: 'SELECT'),
                                SizedBox(width: 14),
                                _PillButton(label: 'START'),
                              ],
                            ),
                            const SizedBox(height: 32),
                            Row(
                              crossAxisAlignment: CrossAxisAlignment.end,
                              children: const [
                                _RoundButton(label: 'B', color: Color(0xFFFF6584)),
                                SizedBox(width: 18),
                                Padding(
                                  padding: EdgeInsets.only(bottom: 14),
                                  child: _RoundButton(
                                    label: 'A',
                                    color: Color(0xFFFF6584),
                                    size: 64,
                                  ),
                                ),
                              ],
                            ),
                          ],
                        ),
                      ],
                    ),
                  ),
                ],
              );
            }
          },
        ),
      ),
    );
  }
}

class _FramePainter extends CustomPainter {
  final ui.Image image;
  _FramePainter(this.image);

  @override
  void paint(Canvas canvas, Size size) {
    paintImage(
      canvas: canvas,
      rect: Rect.fromLTWH(0, 0, size.width, size.height),
      image: image,
      fit: BoxFit.fill,
      filterQuality: FilterQuality.none,
    );
  }

  @override
  bool shouldRepaint(covariant _FramePainter oldDelegate) =>
      oldDelegate.image != image;
}

// ---------- D-Pad ----------

class _DPad extends StatefulWidget {
  const _DPad();

  @override
  State<_DPad> createState() => _DPadState();
}

class _DPadState extends State<_DPad> {
  String? _active;

  void _press(String dir) {
    if (_active == dir) return;
    if (_active != null) setNesButton(_active!, false);
    setState(() => _active = dir);
    HapticFeedback.selectionClick();
    setNesButton(dir, true);
  }

  void _release() {
    if (_active == null) return;
    setNesButton(_active!, false);
    setState(() => _active = null);
  }

  String? _hitTest(Offset local, Size size) {
    final center = Offset(size.width / 2, size.height / 2);
    final d = local - center;
    const dead = 18.0; 
    if (d.distance < dead) return null;
    final angle = d.direction; 
    if (angle > -3 * 3.14159 / 4 && angle <= -3.14159 / 4) return 'UP';
    if (angle > -3.14159 / 4 && angle <= 3.14159 / 4) return 'RIGHT';
    if (angle > 3.14159 / 4 && angle <= 3 * 3.14159 / 4) return 'DOWN';
    return 'LEFT';
  }

  @override
  Widget build(BuildContext context) {
    const size = 132.0;
    return GestureDetector(
      onPanDown: (d) {
        final dir = _hitTest(d.localPosition, const Size(size, size));
        if (dir != null) _press(dir);
      },
      onPanUpdate: (d) {
        final dir = _hitTest(d.localPosition, const Size(size, size));
        if (dir != null) _press(dir);
      },
      onPanEnd: (_) => _release(),
      onPanCancel: _release,
      child: SizedBox(
        width: size,
        height: size,
        child: CustomPaint(painter: _DPadPainter(active: _active)),
      ),
    );
  }
}

class _DPadPainter extends CustomPainter {
  final String? active;
  _DPadPainter({required this.active});

  @override
  void paint(Canvas canvas, Size size) {
    final w = size.width;
    final third = w / 3;

    final base = Paint()
      ..shader = const LinearGradient(
        begin: Alignment.topLeft,
        end: Alignment.bottomRight,
        colors: [Color(0xFF3A3A3D), Color(0xFF222224)],
      ).createShader(Rect.fromLTWH(0, 0, w, w));

    void roundedCross(Paint paint, double inset) {
      final path = Path();
      final r = 6.0;
      path.addRRect(
        RRect.fromRectAndRadius(
          Rect.fromLTWH(third + inset, inset, third - inset * 2, w - inset * 2),
          Radius.circular(r),
        ),
      );
      path.addRRect(
        RRect.fromRectAndRadius(
          Rect.fromLTWH(inset, third + inset, w - inset * 2, third - inset * 2),
          Radius.circular(r),
        ),
      );
      canvas.drawPath(path, paint);
    }

    roundedCross(
      Paint()
        ..color = Colors.black.withOpacity(0.45)
        ..maskFilter = const MaskFilter.blur(BlurStyle.normal, 4),
      2,
    );
    roundedCross(base, 0);

    final rim = Paint()
      ..color = Colors.white.withOpacity(0.06)
      ..style = PaintingStyle.stroke
      ..strokeWidth = 1;
    roundedCross(rim, 0.5);

    if (active != null) {
      final hi = Paint()..color = Colors.white.withOpacity(0.10);
      Rect r;
      switch (active) {
        case 'UP': r = Rect.fromLTWH(third, 0, third, third); break;
        case 'DOWN': r = Rect.fromLTWH(third, third * 2, third, third); break;
        case 'LEFT': r = Rect.fromLTWH(0, third, third, third); break;
        default: r = Rect.fromLTWH(third * 2, third, third, third);
      }
      canvas.drawRect(r, hi);
    }

    final capPaint = Paint()..color = const Color(0xFF161617);
    canvas.drawCircle(Offset(w / 2, w / 2), third * 0.42, capPaint);
  }

  @override
  bool shouldRepaint(covariant _DPadPainter old) => old.active != active;
}

// ---------- A / B round buttons ----------

class _RoundButton extends StatefulWidget {
  final String label;
  final Color color;
  final double size;
  const _RoundButton({
    required this.label,
    required this.color,
    this.size = 56,
  });

  @override
  State<_RoundButton> createState() => _RoundButtonState();
}

class _RoundButtonState extends State<_RoundButton> {
  bool _pressed = false;

  void _set(bool v) {
    setState(() => _pressed = v);
    if (v) HapticFeedback.lightImpact();
    setNesButton(widget.label, v);
  }

  @override
  Widget build(BuildContext context) {
    return GestureDetector(
      onTapDown: (_) => _set(true),
      onTapUp: (_) => _set(false),
      onTapCancel: () => _set(false),
      child: AnimatedContainer(
        duration: const Duration(milliseconds: 60),
        width: widget.size,
        height: widget.size,
        transform: Matrix4.translationValues(0, _pressed ? 3 : 0, 0),
        decoration: BoxDecoration(
          shape: BoxShape.circle,
          gradient: LinearGradient(
            begin: Alignment.topLeft,
            end: Alignment.bottomRight,
            colors: _pressed
                ? [widget.color.withOpacity(0.7), widget.color.withOpacity(0.5)]
                : [widget.color, widget.color.withOpacity(0.75)],
          ),
          boxShadow: _pressed
              ? []
              : [
                  BoxShadow(
                    color: Colors.black.withOpacity(0.6),
                    offset: const Offset(0, 4),
                    blurRadius: 6,
                  ),
                ],
          border: Border.all(color: Colors.black.withOpacity(0.3), width: 1),
        ),
        alignment: Alignment.center,
        child: Text(
          widget.label,
          style: const TextStyle(
            color: Colors.white,
            fontWeight: FontWeight.w800,
            fontSize: 18,
            letterSpacing: 0.5,
          ),
        ),
      ),
    );
  }
}

// ---------- Start / Select pills ----------

class _PillButton extends StatefulWidget {
  final String label;
  const _PillButton({required this.label});

  @override
  State<_PillButton> createState() => _PillButtonState();
}

class _PillButtonState extends State<_PillButton> {
  bool _pressed = false;

  void _set(bool v) {
    setState(() => _pressed = v);
    if (v) HapticFeedback.selectionClick();
    setNesButton(widget.label, v);
  }

  @override
  Widget build(BuildContext context) {
    return GestureDetector(
      onTapDown: (_) => _set(true),
      onTapUp: (_) => _set(false),
      onTapCancel: () => _set(false),
      child: AnimatedContainer(
        duration: const Duration(milliseconds: 60),
        width: 60,
        height: 22,
        transform: Matrix4.translationValues(0, _pressed ? 2 : 0, 0),
        decoration: BoxDecoration(
          color: _pressed ? const Color(0xFF3D3D40) : const Color(0xFF53535A),
          borderRadius: BorderRadius.circular(11),
          boxShadow: _pressed
              ? []
              : [
                  BoxShadow(
                    color: Colors.black.withOpacity(0.4),
                    offset: const Offset(0, 2),
                    blurRadius: 3,
                  ),
                ],
        ),
        alignment: Alignment.center,
        child: Text(
          widget.label,
          style: const TextStyle(
            color: Colors.white70,
            fontSize: 9,
            fontWeight: FontWeight.w700,
            letterSpacing: 0.8,
          ),
        ),
      ),
    );
  }
}
