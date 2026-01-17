// NCAA Calcutta Auction Tracker - Mobile-First App

let CONFIG = null;

const state = {
    prices: {},      // { "East_8_9": 150, ... }
    mine: {},        // { "East_8_9": true, ... }
    budget: 2000,
    activeRegion: 'East'
};

const STORAGE_KEY = 'ncaa_auction_tracker_v2';

// Initialize
async function init() {
    try {
        const response = await fetch('data.json');
        CONFIG = await response.json();
    } catch (e) {
        console.error('Failed to load config:', e);
        CONFIG = getDefaultConfig();
    }

    loadState();
    buildRegionPanels();
    buildTargetsList();
    setupEventListeners();
    updateAll();
}

function getDefaultConfig() {
    return {
        matchups: [
            {id: "8_9", label: "8/9", highSeed: 8, lowSeed: 9},
            {id: "7_10", label: "7/10", highSeed: 7, lowSeed: 10},
            {id: "6_11", label: "6/11", highSeed: 6, lowSeed: 11},
            {id: "5_12", label: "5/12", highSeed: 5, lowSeed: 12},
            {id: "4_13", label: "4/13", highSeed: 4, lowSeed: 13},
            {id: "3_14", label: "3/14", highSeed: 3, lowSeed: 14},
            {id: "2_15", label: "2/15", highSeed: 2, lowSeed: 15},
            {id: "1_16", label: "1/16", highSeed: 1, lowSeed: 16}
        ],
        regions: ["East", "South", "West", "Midwest"],
        expectedValues: {
            "8_9": 1.27, "7_10": 1.48, "6_11": 1.78, "5_12": 1.72,
            "4_13": 2.40, "3_14": 2.93, "2_15": 4.75, "1_16": 8.75
        },
        historicalPriceRatios: {
            "8_9": 1.0, "7_10": 1.15, "6_11": 1.35, "5_12": 1.30,
            "4_13": 1.75, "3_14": 2.10, "2_15": 3.50, "1_16": 6.50
        },
        teams2025: null
    };
}

// Build region panels with matchup cards
function buildRegionPanels() {
    const main = document.getElementById('main');
    main.innerHTML = '';

    CONFIG.regions.forEach(region => {
        const panel = document.createElement('div');
        panel.className = `region-panel ${region === state.activeRegion ? 'active' : ''}`;
        panel.dataset.region = region;

        CONFIG.matchups.forEach(matchup => {
            const card = createMatchupCard(region, matchup);
            panel.appendChild(card);
        });

        main.appendChild(panel);
    });
}

// Calculate adjusted EV based on team's barthag z-score
// Uses additive adjustment: Adj EV = Base EV + (zScore × deltaPerSigma)
// deltaPerSigma values are in percentage points from validated model
function calculateAdjustedEV(region, matchup) {
    const teams = CONFIG.teams2025?.[region]?.[matchup.id];
    const baseEV = CONFIG.expectedValues[matchup.id];

    if (!teams || teams.zScore === undefined) {
        return baseEV;
    }

    const seed = teams.seed || matchup.highSeed;
    const delta = CONFIG.deltaPerSigma?.[seed] || 0.58;

    // Additive adjustment in percentage points
    // e.g., z=+1 for a 1-seed adds 1.74% to their EV
    const adjustedEV = baseEV + (teams.zScore * delta);

    // Floor at a small positive value (team still made tournament)
    return Math.max(adjustedEV, 0.1);
}

// Create a matchup card
function createMatchupCard(region, matchup) {
    const card = document.createElement('div');
    card.className = 'matchup-card';

    const teams = CONFIG.teams2025?.[region]?.[matchup.id] || {
        high: `${matchup.highSeed}-seed`,
        low: `${matchup.lowSeed}-seed`
    };

    const ev = CONFIG.expectedValues[matchup.id];
    const adjEV = calculateAdjustedEV(region, matchup);
    const key = `${region}_${matchup.id}`;

    // Format adjusted EV with color hint
    const adjDiff = adjEV - ev;
    const adjClass = adjDiff > 0.05 ? 'adj-up' : (adjDiff < -0.05 ? 'adj-down' : '');

    card.innerHTML = `
        <div class="matchup-header">
            <span class="matchup-label">${matchup.label}</span>
            <div class="matchup-evs">
                <span class="matchup-ev">EV: ${ev.toFixed(2)}%</span>
                <span class="matchup-adj-ev ${adjClass}">Adj: ${adjEV.toFixed(2)}%</span>
            </div>
        </div>
        <div class="matchup-teams">
            <div class="team-row">
                <span class="team-seed">${matchup.highSeed}</span>
                <span class="team-name">${teams.high}</span>
                <div class="team-input-group">
                    <input type="number" class="price-input"
                           data-key="${key}"
                           placeholder="$0"
                           inputmode="numeric"
                           value="${state.prices[key] || ''}">
                    <button class="mine-btn ${state.mine[key] ? 'active' : ''}"
                            data-key="${key}"
                            aria-label="Mark as mine">
                        ${state.mine[key] ? '★' : '☆'}
                    </button>
                </div>
            </div>
        </div>
    `;

    return card;
}

// Build targets list in menu
function buildTargetsList() {
    const list = document.getElementById('targets-list');
    list.innerHTML = CONFIG.matchups.map(m => `
        <div class="target-item">
            <span class="target-matchup">${m.label}</span>
            <div class="target-values">
                <span class="target-value" data-matchup="${m.id}">$0</span>
                <span class="target-max" data-matchup="${m.id}-max">(max $0)</span>
            </div>
        </div>
    `).join('');
}

// Setup event listeners
function setupEventListeners() {
    // Tab switching
    document.querySelectorAll('.tab').forEach(tab => {
        tab.addEventListener('click', () => switchRegion(tab.dataset.region));
    });

    // Price inputs (delegated)
    document.getElementById('main').addEventListener('input', e => {
        if (e.target.classList.contains('price-input')) {
            handlePriceInput(e.target);
        }
    });

    // Mine buttons (delegated)
    document.getElementById('main').addEventListener('click', e => {
        if (e.target.classList.contains('mine-btn')) {
            handleMineClick(e.target);
        }
    });

    // Budget input
    document.getElementById('budget-input').addEventListener('input', e => {
        state.budget = parseFloat(e.target.value) || 0;
        updateAll();
        saveState();
    });

    // Menu
    document.getElementById('menu-btn').addEventListener('click', () => {
        document.getElementById('menu-overlay').classList.remove('hidden');
    });

    document.getElementById('close-menu').addEventListener('click', () => {
        document.getElementById('menu-overlay').classList.add('hidden');
    });

    document.getElementById('menu-overlay').addEventListener('click', e => {
        if (e.target.id === 'menu-overlay') {
            document.getElementById('menu-overlay').classList.add('hidden');
        }
    });

    // Data buttons
    document.getElementById('clear-btn').addEventListener('click', handleClear);
    document.getElementById('export-btn').addEventListener('click', handleExport);
    document.getElementById('import-btn').addEventListener('click', () => {
        document.getElementById('import-file').click();
    });
    document.getElementById('import-file').addEventListener('change', handleImport);
}

// Switch active region
function switchRegion(region) {
    state.activeRegion = region;

    // Update tabs
    document.querySelectorAll('.tab').forEach(tab => {
        tab.classList.toggle('active', tab.dataset.region === region);
    });

    // Update panels
    document.querySelectorAll('.region-panel').forEach(panel => {
        panel.classList.toggle('active', panel.dataset.region === region);
    });

    saveState();
}

// Handle price input
function handlePriceInput(input) {
    const key = input.dataset.key;
    const value = parseFloat(input.value) || 0;

    if (value > 0) {
        state.prices[key] = value;
        input.classList.add('has-value');
    } else {
        delete state.prices[key];
        input.classList.remove('has-value');
    }

    updateInputStyle(input);
    updateAll();
    saveState();
}

// Handle mine button click
function handleMineClick(btn) {
    const key = btn.dataset.key;
    state.mine[key] = !state.mine[key];

    btn.classList.toggle('active', state.mine[key]);
    btn.textContent = state.mine[key] ? '★' : '☆';

    updateAll();
    saveState();
}

// Update input styling based on EV
function updateInputStyle(input) {
    const key = input.dataset.key;
    const price = state.prices[key];
    const matchupId = key.split('_').slice(1).join('_');
    const estPot = estimateFinalPot();
    const ev = (CONFIG.expectedValues[matchupId] / 100) * estPot.mid;

    input.classList.remove('over-ev');
    if (price && price > ev * 1.2) {
        input.classList.add('over-ev');
    }
}

// Calculate current pot
function calculateCurrentPot() {
    return Object.values(state.prices).reduce((sum, p) => sum + p, 0);
}

// Count items sold
function countItemsSold() {
    return Object.keys(state.prices).filter(k => state.prices[k] > 0).length;
}

// Estimate final pot with confidence interval
function estimateFinalPot() {
    const itemsSold = countItemsSold();
    const currentPot = calculateCurrentPot();

    if (itemsSold === 0) {
        return { low: 15000, mid: 20000, high: 25000 };
    }

    if (itemsSold === 32) {
        return { low: currentPot, mid: currentPot, high: currentPot };
    }

    // Calculate based on what's been sold
    let soldRatioSum = 0;
    let remainingRatioSum = 0;

    CONFIG.matchups.forEach(matchup => {
        CONFIG.regions.forEach(region => {
            const key = `${region}_${matchup.id}`;
            const ratio = CONFIG.historicalPriceRatios[matchup.id];

            if (state.prices[key] > 0) {
                soldRatioSum += ratio;
            } else {
                remainingRatioSum += ratio;
            }
        });
    });

    if (soldRatioSum === 0) {
        return { low: 15000, mid: 20000, high: 25000 };
    }

    const pricePerRatioUnit = currentPot / soldRatioSum;
    const estimatedRemaining = pricePerRatioUnit * remainingRatioSum;
    const mid = Math.round(currentPot + estimatedRemaining);

    // Confidence interval widens with fewer items sold
    const uncertainty = Math.max(0.05, 0.25 * (1 - itemsSold / 32));
    const low = Math.round(mid * (1 - uncertainty));
    const high = Math.round(mid * (1 + uncertainty));

    return { low, mid, high };
}

// Calculate spent on purchases
function calculateSpent() {
    return Object.keys(state.mine)
        .filter(k => state.mine[k])
        .reduce((sum, k) => sum + (state.prices[k] || 0), 0);
}

// Calculate expected payout
function calculateExpectedPayout() {
    const estPot = estimateFinalPot();

    return Object.keys(state.mine)
        .filter(k => state.mine[k])
        .reduce((sum, k) => {
            const matchupId = k.split('_').slice(1).join('_');
            const ev = CONFIG.expectedValues[matchupId] / 100;
            return sum + (ev * estPot.mid);
        }, 0);
}

// Get purchases list
function getPurchases() {
    return Object.keys(state.mine)
        .filter(k => state.mine[k] && state.prices[k] > 0)
        .map(k => {
            const [region, ...parts] = k.split('_');
            const matchupId = parts.join('_');
            const matchup = CONFIG.matchups.find(m => m.id === matchupId);
            const teams = CONFIG.teams2025?.[region]?.[matchupId];

            return {
                key: k,
                name: teams ? `${teams.high} (${region})` : `${region} ${matchup?.label}`,
                price: state.prices[k]
            };
        });
}

// Update all displays
function updateAll() {
    const itemsSold = countItemsSold();
    const currentPot = calculateCurrentPot();
    const estPot = estimateFinalPot();
    const spent = calculateSpent();
    const remaining = state.budget - spent;
    const expectedPayout = calculateExpectedPayout();

    // Header stats
    document.getElementById('items-sold').textContent = `${itemsSold}/32`;
    document.getElementById('current-pot').textContent = formatCurrency(currentPot);

    const remainingEl = document.getElementById('remaining');
    remainingEl.textContent = formatCurrency(remaining);
    remainingEl.style.color = remaining < 0 ? '#e53e3e' : '';

    // Footer pot projection
    document.getElementById('pot-low').textContent = formatShort(estPot.low);
    document.getElementById('pot-mid').textContent = formatCurrency(estPot.mid);
    document.getElementById('pot-high').textContent = formatShort(estPot.high);

    // Update all input styles
    document.querySelectorAll('.price-input').forEach(input => {
        const key = input.dataset.key;
        if (state.prices[key]) {
            input.classList.add('has-value');
        }
        updateInputStyle(input);
    });

    // Targets in menu
    CONFIG.matchups.forEach(m => {
        const ev = CONFIG.expectedValues[m.id];
        const target = Math.round((ev / 100) * estPot.mid);
        const max = Math.round(target * 1.2);

        const targetEl = document.querySelector(`[data-matchup="${m.id}"]`);
        const maxEl = document.querySelector(`[data-matchup="${m.id}-max"]`);

        if (targetEl) targetEl.textContent = formatCurrency(target);
        if (maxEl) maxEl.textContent = `(max ${formatCurrency(max)})`;
    });

    // Purchases in menu
    const purchases = getPurchases();
    const purchasesList = document.getElementById('purchases-list');

    if (purchases.length === 0) {
        purchasesList.innerHTML = '<p class="empty-msg">No purchases yet</p>';
    } else {
        purchasesList.innerHTML = purchases.map(p => `
            <div class="purchase-item">
                <span class="purchase-name">${p.name}</span>
                <span class="purchase-price">${formatCurrency(p.price)}</span>
            </div>
        `).join('');
    }

    document.getElementById('your-total').textContent = formatCurrency(spent);
    document.getElementById('your-ev').textContent = formatCurrency(expectedPayout);

    const roi = spent > 0 ? ((expectedPayout - spent) / spent * 100) : 0;
    const roiEl = document.getElementById('your-roi');
    roiEl.textContent = `${roi >= 0 ? '+' : ''}${roi.toFixed(0)}%`;
    roiEl.style.color = roi >= 0 ? '#48bb78' : '#e53e3e';
}

// Format currency
function formatCurrency(value) {
    return '$' + Math.round(value).toLocaleString();
}

// Format short (for bounds)
function formatShort(value) {
    if (value >= 1000) {
        return '$' + Math.round(value / 1000) + 'k';
    }
    return '$' + value;
}

// Save state
function saveState() {
    try {
        localStorage.setItem(STORAGE_KEY, JSON.stringify(state));
    } catch (e) {
        console.warn('Save failed:', e);
    }
}

// Load state
function loadState() {
    try {
        const saved = localStorage.getItem(STORAGE_KEY);
        if (saved) {
            const parsed = JSON.parse(saved);
            Object.assign(state, parsed);
            document.getElementById('budget-input').value = state.budget;
        }
    } catch (e) {
        console.warn('Load failed:', e);
    }
}

// Clear all
function handleClear() {
    if (confirm('Clear all auction data?')) {
        state.prices = {};
        state.mine = {};

        document.querySelectorAll('.price-input').forEach(input => {
            input.value = '';
            input.classList.remove('has-value', 'over-ev');
        });

        document.querySelectorAll('.mine-btn').forEach(btn => {
            btn.classList.remove('active');
            btn.textContent = '☆';
        });

        updateAll();
        saveState();
        document.getElementById('menu-overlay').classList.add('hidden');
    }
}

// Export
function handleExport() {
    const data = {
        timestamp: new Date().toISOString(),
        state: state
    };

    const blob = new Blob([JSON.stringify(data, null, 2)], { type: 'application/json' });
    const url = URL.createObjectURL(blob);

    const a = document.createElement('a');
    a.href = url;
    a.download = `auction_${new Date().toISOString().split('T')[0]}.json`;
    a.click();

    URL.revokeObjectURL(url);
}

// Import
function handleImport(e) {
    const file = e.target.files[0];
    if (!file) return;

    const reader = new FileReader();
    reader.onload = function(event) {
        try {
            const data = JSON.parse(event.target.result);

            if (data.state) {
                Object.assign(state, data.state);
                document.getElementById('budget-input').value = state.budget;

                // Rebuild UI
                buildRegionPanels();
                setupEventListeners();
                updateAll();
                saveState();

                alert('Data imported!');
                document.getElementById('menu-overlay').classList.add('hidden');
            }
        } catch (err) {
            alert('Import failed: ' + err.message);
        }
    };

    reader.readAsText(file);
    e.target.value = '';
}

// Init on load
document.addEventListener('DOMContentLoaded', init);
