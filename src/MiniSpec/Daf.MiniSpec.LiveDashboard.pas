unit Daf.MiniSpec.LiveDashboard;

interface

const
  LIVE_DASHBOARD_HTML = '''
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>MiniSpec Live</title>
  <script src="https://cdn.tailwindcss.com"></script>
  <script defer src="https://unpkg.com/alpinejs@3.x.x/dist/cdn.min.js"></script>
  <style>
    /* Arrow rotation for details/summary */
    details summary .arrow-icon { transition: transform 0.2s ease; }
    details[open] > summary .arrow-icon { transform: rotate(90deg); }

    /* Print styles */
    @media print {
      body { background: white !important; color: black !important; -webkit-print-color-adjust: exact; print-color-adjust: exact; }
      .no-print { display: none !important; }
      details { display: block !important; }
      /* Show summary but hide arrow and remove pointer */
      details > summary { cursor: default !important; }
      details > summary .arrow-icon { display: none !important; }
      details > div, details > * { display: block !important; }
      details[open] > summary ~ * { display: block !important; }
      /* Force all details open */
      details:not([open]) > summary ~ * { display: block !important; max-height: none !important; }
      /* Reset colors for print */
      .bg-gray-900 { background: white !important; }
      .bg-gray-800, .bg-gray-700 { background: #f3f4f6 !important; }
      .text-white, .text-gray-300, .text-gray-400 { color: #374151 !important; }
      .text-green-400 { color: #059669 !important; }
      .text-red-400 { color: #dc2626 !important; }
      .text-yellow-400 { color: #d97706 !important; }
      .border-green-700 { border-color: #059669 !important; }
      .border-red-700 { border-color: #dc2626 !important; }
      .bg-green-900\/30 { background: #d1fae5 !important; }
      .bg-red-900\/30 { background: #fee2e2 !important; }
      .bg-red-900\/50 { background: #fecaca !important; }
      /* Remove scroll containers */
      .max-h-\[600px\] { max-height: none !important; }
      .overflow-y-auto { overflow: visible !important; }
      /* Ensure page breaks work well */
      .space-y-2 > div { page-break-inside: avoid; }
    }
  </style>
</head>
<body class="bg-gray-900 text-white min-h-screen">
  <!-- SVG Icon Sprites (hidden) -->
  <svg xmlns="http://www.w3.org/2000/svg" class="hidden">
    <symbol id="icon-pass" viewBox="0 0 20 20" fill="currentColor">
      <path fill-rule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zm3.707-9.293a1 1 0 00-1.414-1.414L9 10.586 7.707 9.293a1 1 0 00-1.414 1.414l2 2a1 1 0 001.414 0l4-4z" clip-rule="evenodd"/>
    </symbol>
    <symbol id="icon-fail" viewBox="0 0 20 20" fill="currentColor">
      <path fill-rule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zM8.707 7.293a1 1 0 00-1.414 1.414L8.586 10l-1.293 1.293a1 1 0 101.414 1.414L10 11.414l1.293 1.293a1 1 0 001.414-1.414L11.414 10l1.293-1.293a1 1 0 00-1.414-1.414L10 8.586 8.707 7.293z" clip-rule="evenodd"/>
    </symbol>
    <symbol id="icon-skip" viewBox="0 0 20 20" fill="currentColor">
      <path fill-rule="evenodd" d="M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-8-3a1 1 0 00-.867.5 1 1 0 11-1.731-1A3 3 0 0113 8a3.001 3.001 0 01-2 2.83V11a1 1 0 11-2 0v-1a1 1 0 011-1 1 1 0 100-2zm0 8a1 1 0 100-2 1 1 0 000 2z" clip-rule="evenodd"/>
    </symbol>
    <symbol id="icon-arrow" viewBox="0 0 20 20" fill="currentColor">
      <path fill-rule="evenodd" d="M7.293 14.707a1 1 0 010-1.414L10.586 10 7.293 6.707a1 1 0 011.414-1.414l4 4a1 1 0 010 1.414l-4 4a1 1 0 01-1.414 0z" clip-rule="evenodd"/>
    </symbol>
  </svg>

  <div x-data="dashboard()" x-init="init()" class="container mx-auto p-6">
    <div class="flex justify-between items-center mb-6">
      <h1 class="text-3xl font-bold flex items-center gap-3">
        <svg width="40" height="40" viewBox="0 0 256 256" fill="none" xmlns="http://www.w3.org/2000/svg">
          <rect width="256" height="256" rx="38" fill="#242526"/>
          <text x="128" y="74" text-anchor="middle" font-family="Segoe UI, Arial, sans-serif" font-size="44" fill="#fff" font-weight="bold">mSpec</text>
          <text x="38" y="172" font-family="Fira Mono, Consolas, monospace" font-size="54" fill="#cfcfcf">$&gt;</text>
          <text x="110" y="172" font-family="Segoe UI Emoji" font-size="54" fill="#c92828">?</text>
          <text x="140" y="172" font-family="Fira Mono, Consolas, monospace" font-size="54" fill="#bdbdbd">.</text>
          <text x="160" y="172" font-family="Fira Mono, Consolas, monospace" font-size="54" fill="#bdbdbd">.</text>
          <text x="190" y="172" font-family="Segoe UI Emoji" font-size="54" fill="#34bf4a">?</text>
        </svg>
        <span>MiniSpec Live <span class="text-sm text-gray-500 font-normal">v{{MINISPEC_VERSION}}</span></span>
      </h1>
      <div class="flex items-center gap-4 no-print">
        <span x-show="connected" class="text-green-400 flex items-center gap-1">
          <span class="w-2 h-2 bg-green-400 rounded-full animate-pulse"></span> Connected
        </span>
        <span x-show="!connected" class="text-red-400 flex items-center gap-1">
          <span class="w-2 h-2 bg-red-400 rounded-full"></span> Disconnected
        </span>
      </div>
    </div>

    <!-- Progress Bar -->
    <div class="mb-4 bg-gray-800 rounded-lg p-4">
      <div class="w-full bg-gray-700 rounded-full h-4 overflow-hidden">
        <div class="h-full flex">
          <div class="bg-green-500 transition-all duration-300" :style="`width: ${passPercent}%`"></div>
          <div class="bg-red-500 transition-all duration-300" :style="`width: ${failPercent}%`"></div>
          <div class="bg-yellow-500 transition-all duration-300" :style="`width: ${skipPercent}%`"></div>
        </div>
      </div>
      <div class="flex justify-center gap-6 mt-2 text-sm">
        <span class="text-green-400 flex items-center gap-1"><svg class="w-4 h-4"><use href="#icon-pass"></use></svg> pass: <span x-text="pass"></span></span>
        <span class="text-red-400 flex items-center gap-1"><svg class="w-4 h-4"><use href="#icon-fail"></use></svg> fail: <span x-text="fail"></span></span>
        <span class="text-yellow-400 flex items-center gap-1"><svg class="w-4 h-4"><use href="#icon-skip"></use></svg> skip: <span x-text="skip"></span></span>
        <span class="text-gray-400" x-text="`${totalTests} specs in ${features.length} features`"></span>
        <span class="text-gray-500" x-show="reportComplete &amp;&amp; completedAt" x-text="`at ${completedAt}`"></span>
      </div>
    </div>

    <!-- Filters Panel -->
    <div class="mb-4 bg-gray-800 rounded-lg p-4 no-print">
      <div class="flex flex-wrap items-center gap-4">
        <!-- Status Filters -->
        <div class="flex items-center gap-3">
          <label class="flex items-center gap-1 cursor-pointer">
            <input type="checkbox" x-model="showPass" class="w-4 h-4 accent-green-500">
            <span class="text-green-400 text-sm">Pass</span>
          </label>
          <label class="flex items-center gap-1 cursor-pointer">
            <input type="checkbox" x-model="showFail" class="w-4 h-4 accent-red-500">
            <span class="text-red-400 text-sm">Fail</span>
          </label>
          <label class="flex items-center gap-1 cursor-pointer">
            <input type="checkbox" x-model="showSkip" class="w-4 h-4 accent-yellow-500">
            <span class="text-yellow-400 text-sm">Skip</span>
          </label>
        </div>

        <!-- Regex Filter -->
        <div class="flex items-center gap-2 flex-1 min-w-[200px]">
          <span class="text-gray-400 text-sm">[~]</span>
          <input type="text" x-model="filterRegex" placeholder="Filter by regex..."
                 class="bg-gray-700 border border-gray-600 rounded px-2 py-1 text-sm flex-1 focus:outline-none focus:border-blue-500">
          <button x-show="filterRegex" @click="filterRegex = ''" class="text-gray-400 hover:text-white text-sm">x</button>
        </div>

        <!-- Tag Filter -->
        <div class="flex items-center gap-2">
          <span class="text-gray-400 text-sm">@</span>
          <select x-model="filterTag" class="bg-gray-700 border border-gray-600 rounded px-2 py-1 text-sm focus:outline-none focus:border-blue-500">
            <option value="">All tags</option>
            <template x-for="tag in allTags" :key="tag">
              <option :value="tag" x-text="tag"></option>
            </template>
          </select>
        </div>
      </div>
    </div>

    <!-- Quick Actions: First Failure & Top 10 Slowest -->
    <div class="mb-4 flex gap-4 no-print" x-show="reportComplete">
      <!-- First Failure -->
      <template x-if="firstFailure">
        <button @click="scrollToFeature(firstFailure.featureName)"
                class="bg-red-900/50 border border-red-700 rounded-lg px-4 py-2 text-sm hover:bg-red-900/70 transition flex items-center gap-2">
          <span>!</span>
          <span>First Failure: <span class="font-medium" x-text="firstFailure.scenarioName"></span></span>
        </button>
      </template>

      <!-- Top 10 Slowest Toggle -->
      <button @click="showSlowest = !showSlowest"
              class="bg-yellow-900/50 border border-yellow-700 rounded-lg px-4 py-2 text-sm hover:bg-yellow-900/70 transition flex items-center gap-2">
        <span>&#x231B;</span>
        <span x-text="showSlowest ? 'Hide Slowest' : 'Top 10 Slowest'"></span>
      </button>
    </div>

    <!-- Print Button (visible when report complete) -->
    <div class="mb-4 no-print flex items-center gap-3" x-show="reportComplete">
      <input type="text" x-model="exportFilename" placeholder="report-name"
             class="bg-gray-700 border border-gray-600 rounded px-3 py-2 text-sm w-48 focus:outline-none focus:border-blue-500">
      <button @click="printReport()"
              class="bg-blue-900/50 border border-blue-700 rounded-lg px-4 py-2 text-sm hover:bg-blue-900/70 transition flex items-center gap-2">
        <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
          <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M17 17h2a2 2 0 002-2v-4a2 2 0 00-2-2H5a2 2 0 00-2 2v4a2 2 0 002 2h2m2 4h6a2 2 0 002-2v-4a2 2 0 00-2-2H9a2 2 0 00-2 2v4a2 2 0 002 2zm8-12V5a2 2 0 00-2-2H9a2 2 0 00-2 2v4h10z"></path>
        </svg>
        <span>Export / Print</span>
      </button>
    </div>

    <!-- Top 10 Slowest Panel -->
    <template x-if="showSlowest && reportComplete">
      <div class="mb-4 bg-yellow-900/30 border border-yellow-700 rounded-lg p-4">
        <div class="text-yellow-400 font-medium mb-2">&#x231B; Top 10 Slowest Scenarios</div>
        <div class="space-y-1 text-sm">
          <template x-for="(item, idx) in slowestScenarios" :key="idx">
            <div class="flex items-center gap-2 py-1">
              <span class="text-gray-500 w-6" x-text="`#${idx + 1}`"></span>
              <span class="text-yellow-300 w-16 text-right" x-text="`${item.ms}ms`"></span>
              <svg class="w-4 h-4" :class="item.success ? 'text-green-400' : 'text-red-400'"><use :href="item.success ? '#icon-pass' : '#icon-fail'"></use></svg>
              <span class="text-gray-300 truncate" x-text="item.name"></span>
              <span class="text-gray-500 text-xs" x-text="`(${item.feature})`"></span>
            </div>
          </template>
        </div>
      </div>
    </template>

    <!-- Current Feature -->
    <template x-if="currentFeature">
      <div class="bg-yellow-900/30 border border-yellow-600 rounded-lg p-4 mb-4 animate-pulse">
        <div class="flex items-center gap-2">
          <span class="text-yellow-400 text-xl">></span>
          <span class="font-semibold" x-text="currentFeature"></span>
        </div>
        <div x-show="currentScenario" class="ml-6 mt-2 text-gray-400">
          <span x-text="currentScenario"></span>
        </div>
      </div>
    </template>

    <!-- Completed Features -->
    <div class="space-y-2">
      <template x-for="feature in filteredFeatures" :key="feature.name">
        <div :id="`feature-${feature.name.replace(/\\s+/g, '-')}`"
             :class="feature.success ? 'bg-green-900/30 border-green-700' : 'bg-red-900/30 border-red-700'"
             class="border rounded-lg p-4">
          <details>
            <summary class="cursor-pointer list-none flex justify-between items-center [&::-webkit-details-marker]:hidden">
              <div class="flex items-center gap-2">
                <svg class="arrow-icon w-4 h-4 text-gray-400"><use href="#icon-arrow"></use></svg>
                <svg class="w-5 h-5" :class="feature.success ? 'text-green-400' : 'text-red-400'"><use :href="feature.success ? '#icon-pass' : '#icon-fail'"></use></svg>
                <span class="font-semibold" x-text="feature.name"></span>
                <span class="text-gray-500 text-sm" x-text="`(${feature.pass + feature.fail} examples)`"></span>
              </div>
              <div class="text-gray-400 text-sm flex items-center gap-2">
                <span class="text-green-400 flex items-center gap-0.5"><span x-text="feature.pass"></span><svg class="w-3 h-3"><use href="#icon-pass"></use></svg></span>
                <span class="text-red-400 flex items-center gap-0.5"><span x-text="feature.fail"></span><svg class="w-3 h-3"><use href="#icon-fail"></use></svg></span>
                <span x-text="`${feature.ms}ms`"></span>
              </div>
            </summary>
            <div class="mt-3 ml-4 space-y-3 border-l border-gray-700 pl-4 max-h-[600px] overflow-y-auto">
              <!-- Narrativa del feature -->
              <template x-if="feature.narrative">
                <div class="text-gray-400 text-sm italic whitespace-pre-line" x-text="feature.narrative"></div>
              </template>

              <!-- Background -->
              <template x-if="feature.background && feature.background.length > 0">
                <div class="mb-3">
                  <div class="text-gray-300 font-medium mb-1">Background</div>
                  <div class="ml-4 space-y-0.5 text-xs">
                    <template x-for="(step, idx) in feature.background" :key="idx">
                      <div class="flex items-start gap-2 py-0.5 text-gray-400">
                        <span class="text-blue-400 font-medium w-12 flex-shrink-0" x-text="step.type"></span>
                        <span x-text="step.text"></span>
                      </div>
                    </template>
                  </div>
                </div>
              </template>

              <div x-show="feature.scenarios.length === 0" class="text-gray-500 text-sm italic">No scenarios recorded</div>

              <template x-for="(scenario, idx) in feature.scenarios" :key="idx">
                <div class="mb-3" x-show="(scenario.skipped && showSkip) || (!scenario.skipped && scenario.success && showPass) || (!scenario.skipped && !scenario.success && showFail)">
                  <!-- Scenario Outline con tabla -->
                  <template x-if="scenario.type === 'outline'">
                    <details :open="!scenario.success && !scenario.skipped">
                      <summary class="cursor-pointer list-none flex items-center gap-2 text-sm py-1 [&::-webkit-details-marker]:hidden" :class="scenario.skipped ? 'text-yellow-400' : (scenario.success ? 'text-green-400' : 'text-red-400')">
                        <svg class="arrow-icon w-4 h-4 text-gray-500"><use href="#icon-arrow"></use></svg>
                        <svg class="w-4 h-4" x-show="!scenario.skipped"><use :href="scenario.success ? '#icon-pass' : '#icon-fail'"></use></svg>
                        <svg class="w-4 h-4" x-show="scenario.skipped"><use href="#icon-skip"></use></svg>
                        <span class="font-medium">Scenario Outline:</span>
                        <span x-text="scenario.name" class="flex-1"></span>
                        <span x-show="!scenario.skipped" class="text-gray-500 text-xs flex items-center gap-1"><span x-text="scenario.pass"></span><svg class="w-3 h-3 text-green-400"><use href="#icon-pass"></use></svg> <span x-text="scenario.fail"></span><svg class="w-3 h-3 text-red-400"><use href="#icon-fail"></use></svg></span>
                        <span x-show="scenario.skipped" class="text-gray-500 text-xs">(skip)</span>
                        <span class="text-gray-500 flex-shrink-0" x-text="`${scenario.ms}ms`"></span>
                      </summary>
                      <div class="ml-6 mt-2 space-y-2 text-xs">
                        <!-- Steps template -->
                        <div class="space-y-0.5">
                          <template x-for="(step, stepIdx) in scenario.steps" :key="stepIdx">
                            <div class="flex items-start gap-2 py-0.5 text-gray-400">
                              <span class="text-blue-400 font-medium w-12 flex-shrink-0" x-text="step.type"></span>
                              <span x-text="step.text"></span>
                            </div>
                          </template>
                        </div>
                        <!-- Examples table -->
                        <div class="mt-2">
                          <div class="text-gray-300 font-medium mb-1">Examples:</div>
                          <table class="text-xs border-collapse">
                            <thead>
                              <tr class="text-gray-400">
                                <th class="pr-2 text-left"></th>
                                <template x-for="header in scenario.headers" :key="header">
                                  <th class="px-2 py-1 text-left border-b border-gray-700" x-text="header"></th>
                                </template>
                                <th class="px-2 py-1 text-left border-b border-gray-700">ms</th>
                              </tr>
                            </thead>
                            <tbody>
                              <template x-for="(ex, exIdx) in scenario.examples" :key="exIdx">
                                <tr :class="ex.skipped ? 'text-yellow-400' : (ex.success ? 'text-green-400' : 'text-red-400')">
                                  <td class="pr-2">
                                    <svg class="w-3 h-3" x-show="ex.skipped"><use href="#icon-skip"></use></svg>
                                    <svg class="w-3 h-3" x-show="!ex.skipped"><use :href="ex.success ? '#icon-pass' : '#icon-fail'"></use></svg>
                                  </td>
                                  <template x-for="(val, valIdx) in ex.values" :key="valIdx">
                                    <td class="px-2 py-0.5 border-b border-gray-800" x-text="val"></td>
                                  </template>
                                  <td class="px-2 py-0.5 border-b border-gray-800 text-gray-500" x-text="ex.skipped ? 'skip' : ex.ms"></td>
                                </tr>
                              </template>
                            </tbody>
                          </table>
                        </div>
                        <!-- Errors -->
                        <template x-for="(ex, exIdx) in scenario.examples.filter(e => e.error)" :key="'err'+exIdx">
                          <div class="mt-1 p-2 bg-red-900/50 border border-red-700 rounded text-red-300 font-mono text-xs whitespace-pre-wrap" x-text="ex.error"></div>
                        </template>
                      </div>
                    </details>
                  </template>

                  <!-- Scenario normal -->
                  <template x-if="scenario.type !== 'outline'">
                    <details :open="!scenario.success && !scenario.skipped">
                      <summary class="cursor-pointer list-none flex items-center gap-2 text-sm py-1 [&::-webkit-details-marker]:hidden" :class="scenario.skipped ? 'text-yellow-400' : (scenario.success ? 'text-green-400' : 'text-red-400')">
                        <svg class="arrow-icon w-4 h-4 text-gray-500" x-show="(scenario.steps && scenario.steps.length > 0) || scenario.error"><use href="#icon-arrow"></use></svg>
                        <span class="w-4 h-4" x-show="!(scenario.steps && scenario.steps.length > 0) && !scenario.error"></span>
                        <svg class="w-4 h-4" x-show="!scenario.skipped"><use :href="scenario.success ? '#icon-pass' : '#icon-fail'"></use></svg>
                        <svg class="w-4 h-4" x-show="scenario.skipped"><use href="#icon-skip"></use></svg>
                        <span class="font-medium">Scenario:</span>
                        <span x-text="scenario.name" class="flex-1"></span>
                        <span x-show="scenario.skipped" class="text-gray-500 text-xs">(skip)</span>
                        <span class="text-gray-500 flex-shrink-0" x-text="`${scenario.ms}ms`"></span>
                      </summary>
                      <div class="ml-6 mt-1 space-y-1 text-xs">
                        <template x-if="scenario.steps && scenario.steps.length > 0">
                          <div class="space-y-0.5">
                            <template x-for="(step, stepIdx) in scenario.steps" :key="stepIdx">
                              <div class="flex items-start gap-2 py-0.5" :class="step.success ? 'text-gray-400' : 'text-red-400'">
                                <svg class="w-3 h-3 flex-shrink-0"><use :href="step.success ? '#icon-pass' : '#icon-fail'"></use></svg>
                                <span class="text-blue-400 font-medium w-12 flex-shrink-0" x-text="step.type"></span>
                                <span x-text="step.text" class="flex-1"></span>
                                <span class="text-gray-600" x-text="`${step.ms}ms`"></span>
                              </div>
                            </template>
                          </div>
                        </template>
                        <template x-if="scenario.error">
                          <div class="mt-2 p-2 bg-red-900/50 border border-red-700 rounded text-red-300 font-mono text-xs whitespace-pre-wrap" x-text="scenario.error"></div>
                        </template>
                      </div>
                    </details>
                  </template>
                </div>
              </template>
            </div>
          </details>
        </div>
      </template>
    </div>

  </div>

  <script>
    function dashboard() {
      return {
        connected: false,
        features: [],
        currentFeature: null,
        currentFeatureNarrative: null,
        currentFeatureBackground: null,
        currentFeatureTags: [],
        currentFeatureScenarios: [],
        currentScenario: null,
        pass: 0,
        fail: 0,
        skip: 0,
        completedAt: null,
        reportComplete: false,
        eventSource: null,

        // Filters
        showPass: true,
        showFail: true,
        showSkip: true,
        filterRegex: '',
        filterTag: '',
        showSlowest: false,
        exportFilename: '',

        get totalTests() {
          return this.pass + this.fail + this.skip;
        },
        get executedTests() {
          return this.pass + this.fail;
        },
        get passPercent() {
          const total = this.totalTests;
          return total > 0 ? (this.pass / total * 100) : 0;
        },
        get failPercent() {
          const total = this.totalTests;
          return total > 0 ? (this.fail / total * 100) : 0;
        },
        get skipPercent() {
          const total = this.totalTests;
          return total > 0 ? (this.skip / total * 100) : 0;
        },

        get allTags() {
          const tags = new Set();
          for (const f of this.features) {
            if (f.tags) f.tags.forEach(t => tags.add(t));
            for (const s of f.scenarios || []) {
              if (s.tags) s.tags.forEach(t => tags.add(t));
            }
          }
          return Array.from(tags).sort();
        },

        get filteredFeatures() {
          let regex = null;
          if (this.filterRegex) {
            try { regex = new RegExp(this.filterRegex, 'i'); } catch(e) {}
          }

          return this.features.filter(f => {
            // Tag filter
            if (this.filterTag && (!f.tags || !f.tags.includes(this.filterTag))) {
              // Check scenarios for tag
              const hasTagInScenarios = f.scenarios?.some(s => s.tags?.includes(this.filterTag));
              if (!hasTagInScenarios) return false;
            }

            // Regex filter
            if (regex && !regex.test(f.name) && !regex.test(f.description || '')) {
              const matchesScenario = f.scenarios?.some(s => regex.test(s.name));
              if (!matchesScenario) return false;
            }

            // Status filter
            if (f.success && !this.showPass) return false;
            if (!f.success && !this.showFail) return false;

            return true;
          });
        },

        get firstFailure() {
          for (const f of this.features) {
            for (const s of f.scenarios || []) {
              if (!s.success) {
                return { featureName: f.name, scenarioName: s.name };
              }
            }
          }
          return null;
        },

        get slowestScenarios() {
          const all = [];
          for (const f of this.features) {
            for (const s of f.scenarios || []) {
              if (s.type === 'outline') {
                // Para outlines, usar el tiempo total
                all.push({ name: s.name, ms: s.ms, success: s.success, feature: f.name });
              } else {
                all.push({ name: s.name, ms: s.ms, success: s.success, feature: f.name });
              }
            }
          }
          return all.sort((a, b) => b.ms - a.ms).slice(0, 10);
        },

        scrollToFeature(name) {
          const id = 'feature-' + name.replace(/\\s+/g, '-');
          const el = document.getElementById(id);
          if (el) {
            el.scrollIntoView({ behavior: 'smooth', block: 'center' });
            el.querySelector('details')?.setAttribute('open', '');
          }
        },

        printReport() {
          // Expand all details before printing
          document.querySelectorAll('details').forEach(d => d.setAttribute('open', ''));
          // Set document title for PDF filename
          const originalTitle = document.title;
          const filename = this.exportFilename.trim() || 'MiniSpec-Report';
          document.title = filename;
          // Small delay to let browser update, then print
          setTimeout(() => {
            window.print();
            // Restore title after print dialog
            setTimeout(() => { document.title = originalTitle; }, 500);
          }, 100);
        },

        init() {
          this.connect();
        },

        connect() {
          // Cerrar conexión anterior si existe
          if (this.eventSource) {
            this.eventSource.close();
            this.eventSource = null;
          }

          this.eventSource = new EventSource('/events');

          this.eventSource.onopen = () => {
            this.connected = true;
          };

          this.eventSource.onerror = (e) => {
            this.connected = false;
            if (this.eventSource) {
              this.eventSource.close();
              this.eventSource = null;
            }
            // Reconexión rápida (300ms) para detectar nuevo run casi inmediatamente
            setTimeout(() => this.connect(), 300);
          };

          this.eventSource.onmessage = (e) => {
            const data = JSON.parse(e.data);
            this.handleEvent(data);
          };
        },

        handleEvent(data) {
          switch(data.event) {
            case 'report:start':
              this.features = [];
              this.pass = 0;
              this.fail = 0;
              this.skip = 0;
              this.reportComplete = false;
              this.currentFeatureScenarios = [];
              this.currentFeatureNarrative = null;
              this.currentFeatureTags = [];
              break;
            case 'feature:start':
              this.currentFeature = data.name;
              this.currentFeatureNarrative = data.narrative || null;
              this.currentFeatureBackground = data.background || null;
              this.currentFeatureTags = data.tags || [];
              this.currentScenario = null;
              this.currentFeatureScenarios = [];
              break;
            case 'scenario:start':
              this.currentScenario = data.name;
              break;
            case 'scenario:end':
              // Usar contadores acumulados del servidor
              this.pass = data.totalPass || this.pass;
              this.fail = data.totalFail || this.fail;
              this.skip = data.totalSkip || this.skip;
              this.currentFeatureScenarios.push({
                type: 'scenario',
                name: data.name,
                success: data.success,
                skipped: data.skipped || false,
                ms: data.ms,
                error: data.error || null,
                steps: data.steps || []
              });
              this.currentScenario = null;
              break;
            case 'outline:end':
              // Usar contadores acumulados del servidor
              this.pass = data.totalPass || this.pass;
              this.fail = data.totalFail || this.fail;
              this.skip = data.totalSkip || this.skip;
              // Contar skipped de examples para la UI
              const skipCount = (data.examples || []).filter(e => e.skipped).length;
              this.currentFeatureScenarios.push({
                type: 'outline',
                name: data.name,
                success: data.success,
                skipped: skipCount > 0 && data.pass === 0 && data.fail === 0,
                ms: data.ms,
                pass: data.pass,
                fail: data.fail,
                skip: skipCount,
                headers: data.headers || [],
                steps: data.steps || [],
                examples: data.examples || []
              });
              break;
            case 'feature:end':
              this.features.push({
                name: data.name,
                narrative: this.currentFeatureNarrative,
                background: this.currentFeatureBackground,
                tags: this.currentFeatureTags,
                success: data.fail === 0,
                pass: data.pass,
                fail: data.fail,
                ms: data.ms,
                scenarios: [...this.currentFeatureScenarios]
              });
              this.currentFeature = null;
              this.currentFeatureNarrative = null;
              this.currentFeatureBackground = null;
              this.currentFeatureTags = [];
              this.currentFeatureScenarios = [];
              break;
            case 'report:end':
              this.reportComplete = true;
              this.completedAt = data.completedAt;
              this.pass = data.pass;
              this.fail = data.fail;
              this.skip = data.skip || 0;
              this.currentFeature = null;
              this.currentScenario = null;
              // Mantener conexión abierta para detectar nuevo run
              // Si el servidor cierra, onerror reconectará automáticamente
              break;
          }
        }
      }
    }
  </script>
</body>
</html>
''';

implementation

end.
